package freasymonad

import freasymonad.FreeUtils._

import scala.collection.immutable.Seq
import scala.meta.Ctor.Ref
import scala.meta.Defn.Trait
import scala.meta.Pat.Var
import scala.meta.Term.Name
import scala.meta._
import scala.language.implicitConversions

private[freasymonad] object FreeUtils {
  type Params = Seq[Term.Param]
  type Paramss = Seq[Params]

  // ex: convert (a: A)(b: B) to (a, b)
  val paramssToArgsFlatten: Paramss => Seq[Term.Arg] =
    paramss => paramss.flatten.map(p => Term.Name(p.name.value))

  // todo: implement without out old macros
  import simulacrum._
  @typeclass trait HasName[A] {
    def lift(s: String): A
    def unlift(a: A): String
    def termName(a: A): Term.Name = Term.Name(unlift(a))
    def typeName(a: A): Type.Name = Type.Name(unlift(a))
    def update(a: A, f: String => String): A = lift(f(unlift(a)))
    def capitalize(a: A): A = update(a, _.capitalize)
    def ref(a: A): Ref.Name = Ctor.Ref.Name(unlift(a))
    def patTerm(a: A): Var.Term = Pat.Var.Term(termName(a))
    def termArg(a: A): Term.Arg = termName(a)
    def ===(a: A, tpe: Type): Boolean = tpe.syntax.startsWith(unlift(a))
  }

  object HasName {
    implicit val typeNameInst = new HasName[Type.Name] {
      def unlift(a: Type.Name): String = a.value
      def lift(s: String): Type.Name = Type.Name(s)
    }
    implicit val termNameInst = new HasName[Term.Name] {
      def unlift(a: Term.Name): String = a.value
      def lift(s: String): Term.Name = Term.Name(s)
    }
    implicit val termParamNameInst = new HasName[Term.Param.Name] {
      def unlift(a: Term.Param.Name): String = a.value
      def lift(s: String): Term.Param.Name = Name(s)
    }
    implicit val paramNameInst = new HasName[Type.Param.Name] {
      def unlift(a: Type.Param.Name): String = a.value
      def lift(s: String): Type.Param.Name = Type.Name(s)
    }
    implicit val traitInst = new HasName[Defn.Trait] {
      def unlift(a: Defn.Trait): String = a.name.value
      def lift(s: String): Defn.Trait = abort("Unsupported op for Defn.Trait")
    }
  }

  implicit def liftToSome[A](a: A): Some[A] = Some(a)

  import HasName.ops._

  case class ValDef(isVal: Boolean, name: Term.Name, tparams: Seq[Type.Param], paramss: Paramss, rt: Type, rhs: Option[Term], mods: Seq[Mod] = Nil, pos: Position = Position.None) {
    val outerType: Type = rt match {
      case t"$outer[..$_]" => outer
      case _ => t"Any"
    }
    val innerType: Seq[Type] = rt match {
      case t"$_[..$inner]" => inner
      case _ => Seq.empty
    }
    val isDef: Boolean = !isVal
    def toVal: Defn.Val = q"val ${name.patTerm}: $rt = ${rhs.getOrElse(q"???")}"
    def toDef: Defn.Def = q"def $name[..$tparams](...$paramss): $rt = ${rhs.getOrElse(q"???")}"
    def toExpr: Defn = if (isVal) toVal else toDef
    def toAbsVal: Decl.Val = q"val ${name.patTerm}: $rt"
    def toAbsDef: Decl.Def = q"def $name[..$tparams](...$paramss): $rt"
    def toAbsExpr: Decl = if (isVal) toAbsVal else toAbsDef
  }
  object ValDef {
    def apply(d: Defn.Def): ValDef = d match {
      case q"..$mods def $name[..$tparams](...$paramss): $rt = $rhs" => ValDef(false, name, tparams, paramss, rt.getOrElse(t"Any"), rhs, mods, d.pos)
    }
    def apply(v: Defn.Val): ValDef = v match {
      case q"..$mods val $name: ${rt} = $rhs" => ValDef(true, Term.Name(name.syntax), Nil, Nil, rt.getOrElse(t"Any"), rhs, mods, v.pos)
    }
    def apply(d: Decl.Def): ValDef = d match {
      case q"..$mods def $name[..$tparams](...$paramss): $rt" => ValDef(false, name, tparams, paramss, rt, None, mods, d.pos)
    }
    def apply(v: Decl.Val): ValDef = v match {
      case q"..$mods val $name: $rt" => ValDef(true, name.name, Nil, Nil, rt, None, mods, v.pos)
    }
  }

  def injOpCall(name: Term.Name, typeName: Option[Type.Name] = None, tparams: Seq[Type.Param] = Nil, paramss: Paramss = Nil): Term = {
    val tNames = tparams.map(_.name.typeName)
    val tp = typeName.map(_ +: tNames).getOrElse(tNames)
    val args = paramss.map(_.map(_.name.termArg))
    if (tp.isEmpty) {
      if (paramss.isEmpty) q"injectOps.$name" else q"injectOps.$name(...$args)"
    } else {
      if (paramss.isEmpty) q"injectOps.$name[..$tp]" else q"injectOps.$name[..$tp](...$args)"
    }
  }

  def mkInjOpCall(m: ValDef, typeName: Option[Type.Name] = None): ValDef =
    if (m.isVal) m.copy(rhs = injOpCall(m.name))
    else         m.copy(rhs = injOpCall(m.name, typeName, m.tparams, m.paramss))

  def checkConstraint(members: Seq[ValDef], aliasName: Type.Name): Unit = {
    members.foreach {
      case m if m.rt.syntax == "Any" =>
        abort(s"Define the return type for:\n ${m.toAbsExpr.syntax}")
      case m if m.mods.map(_.syntax).exists(mod => mod == "private" || mod == "protected") =>
        abort(s"try using access modifier 'package-private' for:\n ${m.toAbsExpr.syntax}")
      case m@ValDef(_, _, _, _, _, None, _, _) if !(aliasName === m.outerType) =>
        abort(s"Abstract '${m.toAbsExpr.syntax}' needs to have return type ${aliasName.value}[${m.innerType.mkString(", ")}], otherwise, make it non-abstract.")
      case _ =>
    }
  }

}

private[freasymonad] object FreeImpl {

  def apply(defn: Tree, root: String, imports: Seq[Stat]): Term.Block = {
    import HasName.ops._

    defn match {
      case q"..$_ trait $tname[..$_] extends { ..$_ } with ..$_ { $_ => ..$stats }" =>
        val (alias: Defn.Type, freeS: Type.Name) =
          stats.collectFirst {
            case alias@q"..$_ type $_[..$_] = Free[$s, $_]" =>
              val fixS =  Type.Name(s.syntax.split('.').last) //todo: bug report
              (alias, fixS)
          }.getOrElse(abort("Require a type alias for Free[S, A]"))

        val sealedTrait: Trait =
          stats.collectFirst {
            case t@Defn.Trait(_, name, _, _, _) if name.value == freeS.value => t
          }.getOrElse(abort(s"Require seal trait $freeS[A]"))

        val adt: (Term.Name) => Term.Select =
          name => q"${sealedTrait.termName}.${name.capitalize}"

        val implicitInject = Seq(param"implicit I: Inject[${sealedTrait.name.tpe}, F]")
        val F: Type.Param = tparam"F[_]"

        val members: Seq[ValDef] = stats.collect {
          case d @ Decl.Def(_, _, _, _, _)    => ValDef(d)
          case d @ Defn.Def(_, _, _, _, _, _) => ValDef(d).copy(pos = d.pos)
          case v @ Decl.Val(_, _, _)          => ValDef(v)
          case v @ Defn.Val(_, _, _, _)       => ValDef(v)
          case v @ Decl.Var(_, _, _)          => abort(v.pos, s"var is not allow in @free trait ${tname.value}")
          case v @ Defn.Var(_, _, _, _)       => abort(v.pos, s"var is not allow in @free trait ${tname.value}")
        }

        checkConstraint(members, alias.name)

        val (concreteMem, absMem) = members.partition(_.rhs.isDefined)
        val absMemberOps = absMem.filter(alias.name === _.outerType)

        val (liftedOps: Seq[ValDef], liftedOpsRef: Seq[ValDef]) =
          absMemberOps.collect {
            case m@ValDef(isVal, name, tparams, paramss, _, _, _, _) =>
              val args = paramssToArgsFlatten(paramss)
              val op: ValDef = {
                val rhs = if (isVal) q"Free.liftF(I.inj(${adt(name)}))" else q"Free.liftF(I.inj(${adt(name)}(..$args)))"
                m.copy(tparams = F +: m.tparams, paramss = m.paramss :+ implicitInject, rt = t"Free[F, ..${m.innerType}]", rhs = rhs)
              }
              val opRef: ValDef =
                if (m.isVal) m.copy(rhs = injOpCall(name, sealedTrait.name))
                else         m.copy(rhs = injOpCall(name, sealedTrait.name, tparams, paramss))

              (op, opRef)
          }.unzip

        val (concreteOps: Seq[ValDef], concreteOpsRef: Seq[ValDef], concreteNonOps: Seq[ValDef], concreteNonOpsRef: Seq[ValDef]) = {
          val (ops, nonOps) = concreteMem.partition(alias.name === _.outerType)
          val allOps: Seq[ValDef] = liftedOps ++ ops
          def isCallingOp(name: Term.Name) = allOps.exists(_.name.syntax == name.syntax)
          // append type param F to all call to methods that will be in injectOps Obj (to satisfy F[_]).
          def addFTransformer(term: Term): Term = {
            term.transform {
              case q"${name: Term.Name}[..$tp].$methName(..$args)" if isCallingOp(name) => q"$name[F, ..$tp].$methName(..$args)"
              case q"${name: Term.Name}[..$tp](..$args)"           if isCallingOp(name) => q"$name[F, ..$tp](..$args)"
            } match {
              case t: Term => t
              case t => abort(s"Failed transformation: $t")
            }
          }

          // defs that contain the real implementation
          val injectOps = ops.collect {
            case m@ValDef(_, _, tparams, paramss, _, Some(rhs), _, _) =>
              m.copy(tparams = F +: tparams, paramss = paramss :+ implicitInject, rt = t"Free[F, ..${m.innerType}]", rhs = addFTransformer(rhs))
          }

          // vals and defs that call to defs in injectOps
          val opsRef = ops.map(m => mkInjOpCall(m, sealedTrait.name))

          // vals and defs that call other vals and defs in injectOps
          val nonOpsRef = nonOps.map(m => mkInjOpCall(m))

          (injectOps, opsRef, nonOps, nonOpsRef)
        }

        val opsObj =
          q"""
            object ops {
              $alias
              ..${concreteNonOpsRef.map(_.toExpr)}
              ..${liftedOpsRef.map(_.toExpr)}
              ..${concreteOpsRef.map(_.toExpr)}
            }
          """

        val injectOpsObj =
          q"""
            object injectOps {
              ..${concreteNonOps.map(_.toDef)}
              ..${liftedOps.map(_.toDef)}
              ..${concreteOps.map(_.toDef)}
            }
           """

        val injectClass = {
          // tail to remove the F[_] from tparams; dropRight(1) to remove implicit param
          val opsRef = (liftedOps ++ concreteOps).map(m => mkInjOpCall(m.copy(tparams = m.tparams.tail, paramss = m.paramss.dropRight(1)), t"F"))
          q"""
            class Injects[F[_]](implicit I: Inject[${sealedTrait.name}, F]) {
              ..${opsRef.map(_.toExpr)}
              ..${concreteNonOpsRef.map(_.toExpr)}
            }
            object Injects {
              implicit def injectOps[F[_]](implicit I: Inject[${sealedTrait.name}, F]): Injects[F] = new Injects[F]
            }
           """
        }

        val genCaseClassesAndObjADT = {
          val caseClasses = absMemberOps.map { m =>
            val ext = template"${sealedTrait.ref}[..${m.innerType}]"
            if (m.isVal) q"case object ${m.name.capitalize} extends $ext"
            else         q"case class ${m.name.typeName.capitalize}[..${m.tparams}](..${m.paramss.flatten}) extends $ext"
          }
          q"object ${sealedTrait.termName} { ..$caseClasses }"
        }

        val interp = {
          val cases = absMemberOps.map { m =>
            val binds = m.paramss.flatten.map(p => parg"${p.name.patTerm}")
            val args = m.paramss.map(_.map(_.name.termArg))
            val rhs: Term = if (args.isEmpty) m.name else q"${m.name}(...$args)"
            if (m.isDef) p"case ${adt(m.name)}(..$binds) => $rhs"
            else         p"case ${adt(m.name)} => ${m.name}"
          }
          val op = Type.Name(s"${tname.value}.ops.${alias.name.value}[A]") // full path; better way to do this?
          val monad = Type.Name(s"$root.Monad[M]")
          val run = q"def run[A](op: $op)(implicit m: $monad): M[A] = op.foldMap(this)"
          val methodsToBeImpl = absMemberOps.map(m => m.copy(rt = t"M[..${m.innerType}]").toAbsDef)
          q"""
            trait Interp[M[_]] extends (${tname.termName}.${sealedTrait.name} ~> M) {
              def apply[A](fa: ${tname.termName}.${sealedTrait.name}[A]): M[A] = fa match {
                ..case $cases
              }
              $run
              ..$methodsToBeImpl
            }
           """
        }

        val genCompanionObj =
          q"""
            object ${tname.termName} {
              ..$imports
              $sealedTrait
              $genCaseClassesAndObjADT
              $opsObj
              $injectOpsObj
              ..${injectClass.stats}
              $interp
            }
          """

        Term.Block(Seq(q"trait $tname", genCompanionObj))
      case _ =>
        abort("@free must annotate an trait.")
    }
  }
}

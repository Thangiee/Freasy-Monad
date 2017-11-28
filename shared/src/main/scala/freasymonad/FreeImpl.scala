package freasymonad

import freasymonad.syntax._
import freasymonad.FreeUtils._
import freasymonad.libs.Lib

import scala.collection.immutable.Seq
import scala.language.implicitConversions
import scala.meta.Defn.Trait
import scala.meta.Term.Name
import scala.meta._
import scala.meta.contrib._

private[freasymonad] object libs {
  trait Lib {
    def root: String
    def imports: Seq[Stat]
  }

  val Cats = new Lib {
    val root: String = "cats"
    val imports: Seq[Stat] =
      q"""
        import cats._
        import free._
        import scala.language.higherKinds
      """.stats
  }
  val Scalaz = new Lib {
    val root: String = "scalaz"
    val imports: Seq[Stat] =
      q"""
        import scalaz._
        import scala.language.higherKinds
      """.stats
  }
}

private[freasymonad] object FreeUtils {
  type Params = Seq[Term.Param]
  type Paramss = Seq[Params]

  // ex: convert (a: A)(b: B) to (a, b)
  val paramssToArgsFlatten: Paramss => Seq[Term.Arg] =
    paramss => paramss.flatten.map(p => Term.Name(p.name.value))

  implicit class ParamsOps(val params: Params) extends AnyVal {
    def hasImplicit: Boolean = params match {
      case param"implicit $_: $_" :: _ => true
      case _ => false
    }
  }

  implicit class ParamssOps(val paramss: Paramss) extends AnyVal {
    def dropImplicitParams: Paramss = paramss.filterNot(_.hasImplicit)
  }

  implicit def liftToSome[A](a: A): Some[A] = Some(a)

  case class ValDef(
    mods: Seq[Mod] = Nil,
    isVal: Boolean, name: Name,
    tparams: Seq[Type.Param],
    paramss: Paramss,
    returnType: Option[Type],
    rhs: Option[Term]
  ) {
    val returnTypeOrAny: Type = returnType.getOrElse(t"Any")
    val outerType: Type = returnTypeOrAny match {
      case t"$outer[..$_]" => outer
      case _ => t"Any"
    }
    val innerType: Seq[Type] = returnTypeOrAny match {
      case t"$_[..$inner]" => inner
      case _ => Seq.empty
    }
    val isDef: Boolean = !isVal
    def toVal: Defn.Val = q"val ${name.asPatVarTerm}: $returnTypeOrAny = ${rhs.getOrElse(q"???")}"
    def toDef: Defn.Def = q"def $name[..$tparams](...$paramss): $returnTypeOrAny = ${rhs.getOrElse(q"???")}"
    def toExpr: Defn = if (isVal) toVal else toDef
    def toAbsVal: Decl.Val = q"val ${name.asPatVarTerm}: $returnTypeOrAny"
    def toAbsDef: Decl.Def = q"def $name[..$tparams](...$paramss): $returnTypeOrAny"
    def toAbsExpr: Decl = if (isVal) toAbsVal else toAbsDef
  }
  object ValDef {
    def apply(d: Defn.Def): ValDef = d match {
      case q"..$mods def $name[..$tparams](...$paramss): $_ = $rhs" => ValDef(mods, false, name, tparams, paramss, d.decltpe, rhs)
    }
    def apply(v: Defn.Val): ValDef = v match {
      case q"..$mods val $name: $_ = $rhs" => ValDef(mods, true, Term.Name(name.toString), Nil, Nil, v.decltpe, rhs)
    }
    def apply(d: Decl.Def): ValDef = d match {
      case q"..$mods def $name[..$tparams](...$paramss): $rt" => ValDef(mods, false, name, tparams, paramss, rt, None)
    }
    def apply(v: Decl.Val): ValDef = v match {
      case q"..$mods val $name: $rt" => ValDef(mods, true, name.name, Nil, Nil, rt, None)
    }
    def apply(stats: Seq[Stat]): Seq[ValDef] =
      stats.collect {
        case d @ Decl.Def(_, _, _, _, _)    => ValDef(d)
        case d @ Defn.Def(_, _, _, _, _, _) => ValDef(d)
        case v @ Decl.Val(_, _, _)          => ValDef(v)
        case v @ Defn.Val(_, _, _, _)       => ValDef(v)
        case v @ Decl.Var(_, _, _)          => abort(v.pos, s"var is not allow in @free")
        case v @ Defn.Var(_, _, _, _)       => abort(v.pos, s"var is not allow in @free")
      }
  }

  def injOpCall(name: Term.Name, typeName: Option[Type.Name] = None, tparams: Seq[Type.Param] = Nil, paramss: Paramss = Nil): Term = {
    val tNames = tparams.map(_.name.asType)
    val tp = typeName.map(_ +: tNames).getOrElse(tNames)
    val args = paramss.dropImplicitParams.map(_.map(_.name.asArg))
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
      case m if m.returnType.isEmpty =>
        abort(s"Define the return type for:\n ${m.toAbsExpr}")
      case m if m.mods.map(_.toString).exists(mod => mod == "private" || mod == "protected") =>
        abort(s"try using access modifier 'package-private' for:\n ${m.toAbsExpr}")
      case m if m.rhs.isEmpty && !(aliasName === m.outerType) =>
        val requiredType = s"${aliasName.value}[${m.returnTypeOrAny.toString}]"
        abort(s"Abstract '${m.toAbsExpr}' needs to have return type $requiredType, otherwise, make it non-abstract.")
      case _ =>
    }
  }

}

private[freasymonad] object FreeImpl {

  def apply(defn: Tree, lib: Lib): Term.Block = {

    defn match {
      case q"..$_ trait $tname[..$_] extends { ..$_ } with ..$_ { $_ => ..$stats }" =>
        val (alias: Defn.Type, freeS: Type.Name) =
          stats.collectFirst {
            case alias@q"..$_ type $_[..$_] = Free[$s, $_]" =>
              (alias, s)
          }.getOrElse(abort("Require a type alias for Free[S, A]"))

        val sealedTrait: Trait =
          stats.collectFirst {
            case t@Defn.Trait(_, name, _, _, _) if name.value == freeS.value => t
          }.getOrElse(abort(s"Require seal trait $freeS[A]"))

        val adt: (Term.Name) => Term.Select = name => q"${sealedTrait.name.asTerm}.${name.capitalize.asTerm}"

        val implicitInject = param"implicit I: Inject[${sealedTrait.name.tpe}, F]"
        val F: Type.Param = tparam"F[_]"

        val members: Seq[ValDef] = ValDef(stats)

        checkConstraint(members, alias.name)

        val (concreteMem, absMem) = members.partition(_.rhs.isDefined)
        val absMemberOps = absMem.filter(alias.name === _.outerType)

        val (liftedOps: Seq[ValDef], liftedOpsRef: Seq[ValDef]) =
          absMemberOps.collect {
            case m@ValDef(_, isVal, name, tparams, paramss, _, _) =>
              val op: ValDef = {
                val args = paramssToArgsFlatten(paramss)
                val paramssWithImplInj =
                  m.paramss.reverse match {
                    case (params) :: t if params.hasImplicit => ((params :+ implicitInject) +: t).reverse // group implicitInject with other implicit param
                    case _                                   => m.paramss :+ Seq(implicitInject)
                  }
                val rhs = if (isVal) q"Free.liftF(I.inj(${adt(name)}))" else q"Free.liftF(I.inj(${adt(name)}(..$args)))"
                m.copy(tparams = F +: m.tparams, paramss = paramssWithImplInj, returnType = t"Free[F, ..${m.innerType}]", rhs = rhs)
              }
              val opRef: ValDef =
                if (m.isVal) m.copy(rhs = injOpCall(name, sealedTrait.name))
                else         m.copy(rhs = injOpCall(name, sealedTrait.name, tparams, paramss.dropImplicitParams))

              (op, opRef)
          }.unzip

        val (concreteOps: Seq[ValDef], concreteOpsRef: Seq[ValDef], concreteNonOps: Seq[ValDef], concreteNonOpsRef: Seq[ValDef]) = {
          val (ops, nonOps) = concreteMem.partition(alias.name === _.outerType)
          val allOps: Seq[ValDef] = liftedOps ++ ops
          def isCallingOp(name: Term.Name) = allOps.exists(_.name === name)
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
            case m@ValDef(_, _, _, tparams, paramss, _, Some(rhs)) =>
              m.copy(tparams = F +: tparams, paramss = paramss :+ Seq(implicitInject), returnType = t"Free[F, ..${m.innerType}]", rhs = addFTransformer(rhs))
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
          // tail to remove the F[_] from tparams
          val opsRef = (liftedOps ++ concreteOps).map(m => mkInjOpCall(m.copy(tparams = m.tparams.tail), t"F"))
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

        val caseClassesAndObjADT = {
          val caseClasses = absMemberOps.map { m =>
            val ext = template"${sealedTrait.name.asCtorRef}[..${m.innerType}]"
            if (m.isVal) q"case object ${m.name.capitalize.asTerm} extends $ext"
            else         q"final case class ${m.name.capitalize.asType}[..${m.tparams}](..${m.paramss.flatten.map(_.copy(mods = Nil))}) extends $ext"
          }
          q"object ${sealedTrait.name.asTerm} { ..$caseClasses }"
        }

        val interp = {
          val apply = {
            val cases = absMemberOps.map { m =>
              val binds = m.paramss.flatten.map(p => parg"${p.name.asPatVarTerm}")
              val args = m.paramss.map(_.map(_.name.asArg))
              val rhs: Term = if (args.isEmpty) m.name else q"${m.name}(...$args)"
              if (m.isDef) p"case ${adt(m.name)}(..$binds) => $rhs"
              else p"case ${adt(m.name)} => ${m.name}"
            }
            if (cases.nonEmpty) q"def apply[A](fa: ${tname.asTerm}.${sealedTrait.name}[A]): M[A] = fa match { ..case $cases }"
            else                q"def apply[A](fa: ${tname.asTerm}.${sealedTrait.name}[A]): M[A]"
          }
          val run  = {
            val op = Type.Name(s"${tname.value}.ops.${alias.name.value}[A]") // full path; better way to do this?
            val monad = Type.Name(s"${lib.root}.Monad[M]")
            q"def run[A](op: $op)(implicit m: $monad): M[A] = op.foldMap(this)"
          }
          val methodsToBeImpl = absMemberOps.map(m => m.copy(returnType = t"M[..${m.innerType}]").toAbsDef)
          q"""
            trait Interp[M[_]] extends (${tname.asTerm}.${sealedTrait.name} ~> M) {
              $apply
              $run
              ..$methodsToBeImpl
            }
          """
        }

        val companionObj =
          q"""
            object ${tname.asTerm} {
              ..${lib.imports}
              $sealedTrait
              $caseClassesAndObjADT
              $opsObj
              $injectOpsObj
              ..${injectClass.stats}
              $interp
            }
          """

        Term.Block(Seq(q"trait $tname", companionObj))
      case _ =>
        abort("@free must annotate an trait.")
    }
  }
}

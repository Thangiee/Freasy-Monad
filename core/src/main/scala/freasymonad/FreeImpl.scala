package freasymonad

import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

private[freasymonad] abstract class FreeImpl(val c: blackbox.Context) {
  import c.universe._

  // imports for cats or scalaz
  def imports: Tree

  def runDef(typeAliasName: TypeName): Tree

  def abort(msg: String) = c.abort(c.enclosingPosition, msg)

  // https://issues.scala-lang.org/browse/SI-8771
  def fixSI88771(paramss: Any) = paramss.asInstanceOf[List[List[ValDef]]].map(_.map(_.duplicate))

  def replaceContainerType(tree: Trees#Tree, newType: TypeName): AppliedTypeTree = tree match {
    case AppliedTypeTree(_, inner) => AppliedTypeTree(Ident(newType), inner)
    case other => abort(s"Not an AppliedTypeTree: ${showRaw(other)}")
  }

  def adt(sealedTrait: ClassDef, name: Name) = q"${sealedTrait.name.toTermName}.${TermName(name.toString.capitalize)}"

  type Paramss = List[List[ValDef]]
  def paramssToArgs(paramss: Paramss): List[List[TermName]] =
    paramss.filter(_.nonEmpty).map(_.collect { case t@ValDef(mods, name, _, _) => name })

  // ex: convert (a: A)(b: B) to (a, b)
  def paramssToArgsFlatten(paramss: Paramss): List[TermName] = paramss.flatten.collect { case t@ValDef(mods, name, _, _)  => name }

  // remove () if no args
  def methodCallFmt(method: c.universe.Tree, args: Seq[Seq[TermName]]) = if (args.flatten.isEmpty) method else q"$method(...$args)"

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val trees = annottees.map(_.tree).toList

    trees.headOption match {
      case Some(q"$_ trait ${tpname:TypeName}[..$_] extends { ..$earlydefns } with ..$parents { $self => ..$stats }") =>
        val (typeAlias: TypeDef, freeSType) =
          stats.collectFirst { case typeDef @ q"type $_[..$_] = Free[${Ident(s)}, $_]" => (typeDef, s) }
            .getOrElse(abort("Require a type alias for Free[S, A]"))

        val sealedTrait: ClassDef = stats.collectFirst {
          case cd @ q"sealed trait $name[..$_]" if name == freeSType => cd.asInstanceOf[ClassDef]
        }.getOrElse(abort(s"Require seal trait $freeSType[A]"))

        // create param: (implicit I: free.Inject[?, F])
        val implicitInject = List(ValDef(Modifiers(Flag.IMPLICIT | Flag.PARAM), TermName("I"), tq"Inject[${sealedTrait.name.toTypeName}, F]", EmptyTree))
        val F = q"type F[_]" // higherKind F[_]

        def isReturnTypeOfTypeAlias(rt: Tree): Boolean = rt match {
          case AppliedTypeTree(Ident(name), _)  => name == typeAlias.name
          case _ => false
        }

        // check some constraints that will result in a compiler error
        stats.foreach {
          case x:ValOrDefDef if x.mods.hasFlag(Flag.PRIVATE|Flag.PROTECTED) => c.abort(x.pos, "try using access modifier: package-private")
          case v @ ValDef(_, _, rt: TypeTree, _)       => c.abort(v.pos, s"Define the return type for:") // requires explicit return type
          case d @ DefDef(_, _, _, _, rt: TypeTree, _) => c.abort(d.pos, s"Define the return type for:") // requires explicit return type
          case v @ ValDef(mods, _, rt, _) if mods.hasFlag(Flag.MUTABLE) => c.abort(v.pos, s"var is not allow in @free trait $tpname")
          case v @ ValDef(_, _, rt, EmptyTree)  =>
            if (!isReturnTypeOfTypeAlias(rt)) c.abort(v.pos, s"Abstract val needs to have return type ${typeAlias.name}[...], otherwise, make it non-abstract.")
          case d @ DefDef(_, _, _, _, rt, EmptyTree) =>
            if (!isReturnTypeOfTypeAlias(rt)) c.abort(d.pos, s"Abstract def needs to have return type ${typeAlias.name}[...], otherwise, make it non-abstract.")
          case _ => // no issue
        }

        val absValsDefsOps: Seq[ValOrDefDef] = stats.collect {
          case m @ DefDef(_, _, _, _, AppliedTypeTree(Ident(typ), _), EmptyTree) if typ == typeAlias.name => m
          case v @ ValDef(_, _, AppliedTypeTree(Ident(typ), _), EmptyTree) if typ == typeAlias.name => v
        }

        // --------------------------------------------------------------------------------
        // vals name with op(s) means having return type matching the defined typeAlias.
        // And vise versa, nonOp(s) means having different return type than the typeAlias.
        // --------------------------------------------------------------------------------

        val (liftedOps:Seq[DefDef], liftedOpsRef:Seq[ValOrDefDef]) = absValsDefsOps.map {
          case DefDef(_, name, tparams, paramss, rt@AppliedTypeTree(_, innerType), _) =>
            val op = {
              val args = paramssToArgsFlatten(paramss)
              val rhs = q"Free.liftF(I.inj(${adt(sealedTrait, name)}(..$args)))"
              val params = (if (paramss.isEmpty) List.empty else paramss) :+ implicitInject
              q"def $name[..${F +: tparams}](...$params): Free[F, ..$innerType] = $rhs".asInstanceOf[DefDef]
            }
            val opRef = {
              val args = paramssToArgs(paramss)
              val rhs = methodCallFmt(q"injectOps.$name[..${sealedTrait.name +: tparams.map(_.name)}]", args)
              DefDef(Modifiers(), name, tparams, paramss, rt, rhs)
            }
            (op, opRef)
          case ValDef(_, name, rt@AppliedTypeTree(_, innerType), rhs) =>
            val op = {
              val rhs = q"Free.liftF(I.inj(${adt(sealedTrait, name)}))"
              q"def $name[$F](..$implicitInject): Free[F, ..$innerType] = $rhs".asInstanceOf[DefDef]
            }
            val opRef = ValDef(Modifiers(), name, rt, q"injectOps.$name[${sealedTrait.name}]")
            (op, opRef)
        }.unzip

        val concreteValsDefs: Seq[ValOrDefDef] = stats.collect {
          case m @ DefDef(_, _, _, _, _, rhs) if rhs.nonEmpty => m
          case v @ ValDef(_, _, _, rhs)       if rhs.nonEmpty => v
        }

        val (concreteOps: Seq[DefDef], concreteOpsRef: Seq[ValOrDefDef], concreteNonOps: Seq[ValOrDefDef], concreteNonOpsRef: Seq[ValOrDefDef]) = {
          val (ops, nonOps) = concreteValsDefs.partition {
            case DefDef(_, _, _, _, AppliedTypeTree(Ident(outerType), _), _) => outerType == typeAlias.name
            case ValDef(_, _, AppliedTypeTree(Ident(outerType), _), _)       => outerType == typeAlias.name
            case _ => false
          }

          // append type param F to all call to methods that will be in injectOps Obj (to satisfy F[_]).
          val addFTransformer = new Transformer {
            val injectOps = liftedOps ++ ops
            override def transform(tree: c.universe.Tree): c.universe.Tree = tree match {
              case Apply(Select(TypeApply(Ident(name:TermName), tp), name2), args) if injectOps.exists(_.name == name) =>
                super.transform(Apply(Select(TypeApply(Ident(name), tq"F" :: tp), name2), args))
              case Apply(TypeApply(Ident(name:TermName), tp), args) if injectOps.exists(_.name == name) =>
                super.transform(q"$name[..${tq"F" +: tp}](..$args)")
              case _ =>
                super.transform(tree)
            }
          }

          // defs that contain the real implementation
          val injectOps: Seq[DefDef] = ops.map {
            case DefDef(mods, tname, tp, paramss, AppliedTypeTree(_, innerType), rhs) =>
              q"$mods def $tname[..${F +: tp}](...${paramss :+ implicitInject}): Free[F, ..$innerType] = ${addFTransformer.transform(rhs)}".asInstanceOf[DefDef]
            case ValDef(mods, tname, AppliedTypeTree(_, innerType), rhs) =>
              q"$mods def $tname[$F](..$implicitInject): Free[F, ..$innerType] = ${addFTransformer.transform(rhs)}".asInstanceOf[DefDef]
          }

          // vals and defs that call to defs in injectOps
          val opsRef = ops.map {
            case DefDef(mods, name, tparams, paramss, rt, _) =>
              val rhs = methodCallFmt(q"injectOps.$name[..${sealedTrait.name +: tparams.map(_.name)}]", paramssToArgs(paramss))
              DefDef(mods, name, tparams, paramss, rt, rhs)
            case ValDef(mods, name, rt, _) =>
              ValDef(mods, name, rt, q"injectOps.$name[${sealedTrait.name}]")
          }

          // vals and defs that call other vals and defs in injectOps
          val nonOpsRef = nonOps.map {
            case DefDef(mods, name, tparams, paramss, rt, _) =>
              val rhs = methodCallFmt(q"injectOps.$name[..${tparams.map(_.name)}]", paramssToArgs(paramss))
              DefDef(mods, name, tparams, paramss, rt, rhs)
            case ValDef(mods, name, rt, _) =>
              ValDef(mods, name, rt, q"injectOps.$name")
          }

          (injectOps, opsRef, nonOps, nonOpsRef)
        }

        val opsObj = {
          q"""
            object ops {
              $typeAlias
              ..$concreteNonOpsRef
              ..$liftedOpsRef
              ..$concreteOpsRef
            }
          """
        }

        val injectOpsObj = {
          q"""
            object injectOps {
              ..$concreteNonOps
              ..$liftedOps
              ..$concreteOps
            }
           """
        }

        val injectClass = {
          val opsRef = (liftedOps ++ concreteOps).map {
            case q"$_ def $tname[..${tparams:List[TypeDef]}](...${paramss:Paramss}): $tpt = $_" =>
              val rhs = methodCallFmt(q"injectOps.$tname[..${tparams.map(_.name)}]", paramssToArgs(paramss.dropRight(1)))
              // tail to remove the F[_] from tparams; dropRight(1) to remove implicit param
              q"def $tname[..${tparams.tail}](...${paramss.dropRight(1)}): $tpt = $rhs"
          }
          q"""
            class Injects[F[_]](implicit I: Inject[${sealedTrait.name}, F]) {
              ..$opsRef
              ..$concreteNonOpsRef
            }

            object Injects {
              implicit def injectOps[F[_]](implicit I: Inject[${sealedTrait.name}, F]): Injects[F] = new Injects[F]
            }
           """
        }

        val methodsToBeImpl: Seq[DefDef] = absValsDefsOps.map {
          case DefDef(mods, name, tparams, paramss, rt, _) =>
            DefDef(mods, name, tparams, paramss, replaceContainerType(rt, TypeName("M")), EmptyTree)
          case ValDef(mods, name, rt, _) =>
            DefDef(mods, name, List.empty, List.empty, replaceContainerType(rt, TypeName("M")), EmptyTree)
        }

        val genCaseClassesAndObjADT = {
          val caseClasses = absValsDefsOps.collect {
            case q"$_ def $tname[..$tparams](...$paramss): ${AppliedTypeTree(_, returnType)} = $expr" =>
              q"case class ${TypeName(tname.toString.capitalize)}[..$tparams](..${fixSI88771(paramss).flatten}) extends ${sealedTrait.name}[..$returnType]"
            case ValDef(_, name, AppliedTypeTree(_, returnType), _) =>
              q"case object ${TermName(name.toString.capitalize)} extends ${sealedTrait.name}[..$returnType]"
          }
          q"object ${sealedTrait.name.toTermName} { ..$caseClasses }"
        }

        val genCompanionObj =
          q"""
            object ${tpname.toTermName} {
              ..$imports
              import scala.language.higherKinds
              $sealedTrait
              $genCaseClassesAndObjADT
              ..$opsObj
              ..$injectOpsObj
              ..$injectClass
              trait Interp[M[_]] {
                import ops._
                val interpreter = new (${sealedTrait.name} ~> M) {
                  def apply[A](fa: ${sealedTrait.name}[A]): M[A] = fa match {
                    case ..${absValsDefsOps.map {
                      case DefDef(_, name, _, paramss, rt, _) =>
                        val binds = paramss.flatMap(_.collect { case t:ValDef => Bind (t.name, Ident(termNames.WILDCARD))})
                        val args = paramss.map(_.collect { case t:ValDef => Ident(t.name.toTermName) })
                        val rhs = if (args.isEmpty) q"$name" else q"$name(...$args)"
                        cq"${adt(sealedTrait, name)}(..$binds) => $rhs"
                      case ValDef(_, name, _, _) =>
                        cq"${adt(sealedTrait, name)} => $name"
                    }}
                  }
                }
                ${runDef(typeAlias.name)}
                ..$methodsToBeImpl
              }
            }
           """

        val gen = q"..${List(q"trait $tpname", genCompanionObj)}"
//        println(showCode(gen))
        c.Expr[Any](gen)

      case other => c.abort(c.enclosingPosition, s"${showRaw(other)}")
    }
  }

}

package freasymonad

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class free extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro freeImpl.impl
}

object freeImpl {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val trees = annottees.map(_.tree).toList

    def abort(msg: String) = c.abort(c.enclosingPosition, msg)

    // https://issues.scala-lang.org/browse/SI-8771
    def fixSI88771(paramss: Any) = paramss.asInstanceOf[List[List[ValDef]]].map(_.map(_.duplicate))

    def replaceContainerType(tree: Trees#Tree, newType: TypeName): AppliedTypeTree = tree match {
      case AppliedTypeTree(_, inner) => AppliedTypeTree(Ident(newType), inner)
      case other => abort(s"Not an AppliedTypeTree: ${showRaw(other)}")
    }

    def methodToCaseClassADT(sealedTrait: ClassDef, methodName: Name) =
      q"${sealedTrait.name.toTermName}.${TermName(methodName.toString.capitalize)}"

    type Paramss = List[List[ValDef]]
    def paramssToArgs(paramss: Paramss): List[List[TermName]] =
      paramss.filter(_.nonEmpty).map(_.collect { case t@ValDef(mods, name, _, _) => name })

    // ex: convert (a: A)(b: B) to (a, b)
    def paramssToArgsFlatten(paramss: Paramss): List[TermName] =
      paramss.flatten.collect { case t@ValDef(mods, name, _, _)  => name }

    // remove () if no args
    def methodCallFmt(method: c.universe.Tree, args: Seq[Seq[TermName]]): c.universe.Tree =
      if (args.flatten.isEmpty) method else q"$method(...$args)"

    trees.headOption match {
      case Some(q"$_ trait ${tpname:TypeName}[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }") =>
        val (typeAlias: TypeDef, freeSType) =
          stats.collectFirst { case typeDef @ q"type $_[..$_] = Free[${Ident(s)}, $_]" => (typeDef, s) }
            .getOrElse(abort("Require a type alias for Free[S, A]"))

        val sealedTrait: ClassDef = stats.collectFirst {
          case cd @ q"sealed trait $name[..$_]" if name == freeSType => cd.asInstanceOf[ClassDef]
        }.getOrElse(abort(s"Require seal trait $freeSType[A]"))

        // create param: (implicit I: free.Inject[?, F])
        val implicitInject = List(ValDef(Modifiers(Flag.IMPLICIT | Flag.PARAM), TermName("I"), tq"free.Inject[${sealedTrait.name.toTypeName}, F]", EmptyTree))
        val F = q"type F[_]" // higherKind F[_]

        val concreteMethods = stats.collect { case m@DefDef(_, _, _, _, _, rhs) if rhs.nonEmpty => m }

        val concreteInjectOps = concreteMethods.collect {
          case q"$_ def $tname[..$tparams](...${paramss:Paramss}): ${tpt@AppliedTypeTree(Ident(outerType), innerType)} = $_" if outerType == typeAlias.name =>
            val args = paramssToArgs(paramss)
            val rhs =
              q"""
                ${methodCallFmt(q"ops.$tname", args)}.compile(new arrow.FunctionK[${sealedTrait.name}, F] {
                  def apply[A](fa: ${sealedTrait.name}[A]): F[A] = I.inj(fa)
                })
              """

            q"def $tname[..${F +: tparams}](...${paramss :+ implicitInject}): free.Free[F, ..$innerType] = $rhs"
        }

        val absMethods: Seq[c.universe.DefDef] = stats.collect {
          case m@DefDef(_, _, _, _, AppliedTypeTree(Ident(typ), _), EmptyTree) if typ == typeAlias.name => m
        }

        val (liftedOps, liftedInjectOps) = absMethods.map {
          case q"${mods:Modifiers} def ${tname:TermName}[..${tparams:List[TypeDef]}](...${paramss: Paramss}): ${tpt@AppliedTypeTree(_, innerType)} = $_" =>
            val op = {
              val args = paramssToArgs(paramss)
              val rhs = methodCallFmt(q"injectOps.$tname[..${sealedTrait.name +: tparams.map(_.name)}]", args)
              q"def $tname[..$tparams](...$paramss): $tpt = $rhs"
            }

            val injectOp = {
              val args = paramssToArgsFlatten(paramss)
              val rhs = q"free.Free.inject[${sealedTrait.name}, F](${methodToCaseClassADT(sealedTrait, tname)}(..$args))"
              val params = (if (paramss.isEmpty) List.empty else paramss) :+ implicitInject
              q"def $tname[..${F +: tparams}](...$params): free.Free[F, ..$innerType] = $rhs"
            }

            (op, injectOp)
        }.unzip

        val injectClass = {
          val methods = (liftedInjectOps ++ concreteInjectOps).map {
            case q"$_ def $tname[..${tparams:List[TypeDef]}](...${paramss:Paramss}): $tpt = $_" =>
              val rhs = methodCallFmt(q"injectOps.$tname[..${tparams.map(_.name)}]", paramssToArgs(paramss))
              // tail to remove the F[_] from tparams; dropRight(1) to remove implicit param
              q"def $tname[..${tparams.tail}](...${paramss.dropRight(1)}): $tpt = $rhs"
          }

          q"""
            class Inject[F[_]](implicit I: free.Inject[${sealedTrait.name}, F]) {
              ..$methods
            }

            object Inject {
              implicit def injectOps[F[_]](implicit I: free.Inject[${sealedTrait.name}, F]): Inject[F] = new Inject[F]
            }
           """
        }

        val methodsToBeImpl = absMethods.map(m => q"def ${m.name}[..${m.tparams}](...${fixSI88771(m.vparamss)}): ${replaceContainerType(m.tpt, TypeName("M"))}")

        val genCaseClassesADT = {
          val caseClasses = absMethods.collect {
            case q"$_ def $tname[..$tparams](...$paramss): ${AppliedTypeTree(_, returnType)} = $expr" =>
              q"case class ${TypeName(tname.toString.capitalize)}[..$tparams](..${fixSI88771(paramss).flatten}) extends ${sealedTrait.name}[..$returnType]"
          }
          q"object ${sealedTrait.name.toTermName} { ..$caseClasses }"
        }

        val genCompanionObj =
          q"""
            object ${tpname.toTermName} {
              import cats._
              import scala.language.higherKinds
              $sealedTrait
              $genCaseClassesADT
              object ops {
                $typeAlias
                ..$liftedOps
                ..$concreteMethods
              }
              object injectOps {
                ..$liftedInjectOps
                ..$concreteInjectOps
              }
              ..$injectClass
              trait Interp[M[_]] {
                import ops._
                val interpreter = new (${sealedTrait.name} ~> M) {
                  def apply[A](fa: ${sealedTrait.name}[A]): M[A] = fa match {
                    case ..${absMethods.map {m =>
                      val binds = m.vparamss.flatMap(_.collect { case t:ValDef => Bind (t.name, Ident(termNames.WILDCARD))})
                      val args = m.vparamss.map(_.collect { case t:ValDef => Ident(t.name.toTermName) })
                      val rhs = if (args.isEmpty) q"${m.name}" else q"${m.name}(...$args)"
                      cq"${methodToCaseClassADT(sealedTrait, m.name)}(..$binds) => $rhs"
                    }}
                  }
                }
                def run[A](op: ${typeAlias.name}[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] = op.foldMap(interpreter)
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

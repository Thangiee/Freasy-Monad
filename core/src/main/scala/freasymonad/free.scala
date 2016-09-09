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

    trees.headOption match {
      case Some(q"$mods trait ${tpname:TypeName}[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }") =>

        val (typeAlias: TypeDef, freeSType) =
          stats.collectFirst { case typeDef @ q"type $_[..$_] = Free[${Ident(s)}, $_]" => (typeDef, s) }
            .getOrElse(abort("Require a type alias for Free[S, A]"))

        val sealedTrait: ClassDef = stats.collectFirst {
          case cd @ q"sealed trait $name[..$_]" if name == freeSType => cd.asInstanceOf[ClassDef]
        }.getOrElse(abort(s"Require seal trait $freeSType[A]"))

        val concreteMethods = stats.collect { case m@DefDef(_, _, _, _, _, rhs) if rhs.nonEmpty => m }

        val absMethods: Seq[c.universe.DefDef] = stats.collect {
          case m@DefDef(_, _, _, _, AppliedTypeTree(Ident(typ), _), EmptyTree) if typ == typeAlias.name => m
        }

        val liftedMethods = absMethods.map {
          case q"$_ def ${tname@TermName(name)}[..$tparams](...$paramss): ${tpt@AppliedTypeTree(_, innerType)} = $_" =>
            val tpe = tparams.collect {  case t:TypeDef => Ident(t.name) }
            val args = paramss.head.collect { case t:ValDef => Ident(t.name.toTermName) }
            val rhs =
              q"""
                cats.free.Free.liftF[${sealedTrait.name}, ..$innerType](
                  ${methodToCaseClassADT(sealedTrait, tname)}[..$tpe](..$args)
                )
              """

            q"def $tname[..$tparams](...$paramss): $tpt = $rhs"
        }

        val methodsToBeImpl = absMethods.map(m => q"def ${m.name}[..${m.tparams}](...${fixSI88771(m.vparamss)}): ${replaceContainerType(m.tpt, TypeName("M"))}")

        val genCaseClassesADT = {
          val caseClasses = absMethods.collect {
            case q"$_ def $tname[..$tparams](...$paramss): ${AppliedTypeTree(_, returnType)} = $expr" =>
              q"case class ${TypeName(tname.toString.capitalize)}[..$tparams](...${fixSI88771(paramss)}) extends ${sealedTrait.name}[..$returnType]"
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
                ..$liftedMethods
                ..$concreteMethods
              }

              trait Interp[M[_]] {
                import ops._
                val interpreter = new (${sealedTrait.name} ~> M) {
                  def apply[A](fa: ${sealedTrait.name}[A]): M[A] = fa match {
                    case ..${absMethods.map {m =>
                      val binds = m.vparamss.head.collect { case t:ValDef => Bind (t.name, Ident(termNames.WILDCARD))}
                      val args = m.vparamss.head.collect { case t:ValDef => Ident(t.name.toTermName) }
                      cq"${methodToCaseClassADT(sealedTrait, m.name)}(..$binds) => ${m.name}(..$args)"
                    }}
                  }
                }

                def run[A](op: ${typeAlias.name}[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] = op.foldMap(interpreter)

                ..$methodsToBeImpl
              }
            }
           """

        val gen = q"..${List(q"trait $tpname", genCompanionObj)}"
        println(showCode(gen))
        c.Expr[Any](gen)

      case other => c.abort(c.enclosingPosition, s"${showRaw(other)}")
    }
  }

}

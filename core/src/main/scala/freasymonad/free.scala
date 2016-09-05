package freasymonad

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

class free extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro freeImpl.impl
}

object freeImpl {

  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val trees = annottees.map(_.tree).toList

    def replaceContainerType(tree: Trees#Tree, newType: TypeName): AppliedTypeTree = tree match {
      case AppliedTypeTree(_, inner) => AppliedTypeTree(Ident(newType), inner)
      case other => c.abort(c.enclosingPosition, s"Not an AppliedTypeTree: ${showRaw(other)}")
    }

    def extractFreeTypeAlias(stats: Seq[Trees#Tree]): TypeDef = stats.collectFirst[TypeDef] {
      case t@TypeDef(_, _, _, AppliedTypeTree(Ident(TypeName("Free")), _)) => t
    }.getOrElse(c.abort(c.enclosingPosition, s"Require a type alias for Free[S, A]"))

    def extractADTSealedTrait(stats: Seq[Trees#Tree]): ClassDef = stats.collectFirst[ClassDef] {
      case t:ClassDef if t.mods.hasFlag(Flag.SEALED) && t.mods.hasFlag(Flag.TRAIT) => t
    }.getOrElse(c.abort(c.enclosingPosition, s"Require a sealed trait"))

    def extractMethodsWithImp(stats: Seq[Trees#Tree]): Seq[DefDef] = stats.collect {
      case m@DefDef(_, _, _, _, _, tree) if tree.nonEmpty => m
    }

    def extractMethodsWithNoImpl(stats: Seq[Trees#Tree]): Seq[DefDef] = stats.collect {
      case m@DefDef(_, _, _, _, _, EmptyTree) => m
    }

    def genGrammarADT(sealTrait: ClassDef, methods: Seq[DefDef]) = {
      val caseClasses = methods.collect {
        case q"$_ def $tname[..$tparams](...$paramss): ${AppliedTypeTree(_, returnType)} = $expr" =>
          q"case class ${TypeName(tname.toString.capitalize)}[..$tparams](...$paramss) extends ${sealTrait.name}[..$returnType]"
      }
      q"""
         object ${sealTrait.name.toTermName} {
           ..$caseClasses
         }
       """
    }

    val result: c.universe.Tree = trees.headOption match {
      case Some(q"$mods trait ${tpname:TypeName}[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }") =>
        val typeAlias: c.universe.TypeDef = extractFreeTypeAlias(stats)
        val freeSType = typeAlias.rhs match {
          case AppliedTypeTree(_, Ident(s) :: a) => s.toTypeName
        }

        val sealTrait = extractADTSealedTrait(stats)
        if (sealTrait.name != freeSType) c.abort(c.enclosingPosition, s"Require seal trait ${freeSType.toString}[A]")

        val methods = extractMethodsWithNoImpl(stats).collect {
          case m@DefDef(_, _, _, _, AppliedTypeTree(Ident(typ), _), _) if typ == typeAlias.name => m
        }
        val grammar = genGrammarADT(sealTrait, methods)

        val liftedMethods = methods.map {
          case q"$_ def ${tname@TermName(name)}[..$tparams](...$paramss): ${tpt@AppliedTypeTree(_, innerType)} = $_" =>
            val tpe = tparams.collect {  case t:TypeDef => Ident(t.name) }
            val args = paramss.head.collect { case t:ValDef => Ident(t.name.toTermName) }
            val rhs =
              q"""
                cats.free.Free.liftF[${sealTrait.name}, ..$innerType](
                  ${sealTrait.name.toTermName}.${TermName(tname.toString.capitalize)}[..$tpe](..$args)
                )
              """

            q"def $tname[..$tparams](...$paramss): $tpt = $rhs"
        }

        val res =
          q"""
            import cats.free.Free

            $mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self =>
              $typeAlias
              $sealTrait
              ..$grammar
              ..$liftedMethods
              ..${extractMethodsWithImp(stats)}
            }

             object ${tpname.toTermName} extends $tpname {
               import cats._
               import scala.language.higherKinds
               trait Interpreter[M[_]] {
                 val interpreter = new (${sealTrait.name} ~> M) {
                   def apply[A](fa: ${sealTrait.name}[A]): M[A] = fa match {
                     case ..${methods.map {m =>
                        val binds = m.vparamss.head.collect { case t:ValDef => Bind (t.name, Ident(termNames.WILDCARD))}
                        val args = m.vparamss.head.collect { case t:ValDef => Ident(t.name.toTermName) }
                        cq"ADT.${TermName(m.name.toString.capitalize)}(..$binds) => ${m.name}(..$args)"
                     }}
                   }
                 }

                 def run[A](op: ${typeAlias.name}[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] =
                  op.foldMap(interpreter)

                 ..${methods.map(m => q"def ${m.name}[..${m.tparams}](...${m.vparamss}): ${replaceContainerType(m.tpt, TypeName("M"))}")}
               }
             }
          """

         val out = res
        println(showCode(out))

        out
      case other => c.abort(c.enclosingPosition, s"${showRaw(other)}")
    }

    c.Expr[Any](result)
  }

}

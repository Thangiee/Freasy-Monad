package freasymonad.cats

import freasymonad.FreeImpl

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class free extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro CatsFreeImpl.impl
}

class CatsFreeImpl(ctx: blackbox.Context) extends FreeImpl(ctx) {
  import c.universe._

  val imports: Tree =
    q"""
       import cats._
       import cats.free._
     """

  def runDef(typeAliasName: TypeName): Tree =
    q"def run[A](op: $typeAliasName[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] = op.foldMap(interpreter)"
}

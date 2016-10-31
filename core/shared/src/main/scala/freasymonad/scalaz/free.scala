package freasymonad.scalaz

import freasymonad.FreeImpl

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class free extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ScalazFreeImpl.impl
}

class ScalazFreeImpl(ctx: blackbox.Context) extends FreeImpl(ctx) {
  import c.universe._
  val imports: Tree = q"import scalaz._"
}

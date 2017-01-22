package freasymonad.scalaz

import freasymonad.FreeImpl

import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("enable macro paradise to expand macro annotations")
class free extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    FreeImpl.apply(defn, freasymonad.libs.Scalaz)
  }
}

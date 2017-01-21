package freasymonad.scalaz

import freasymonad.FreeImpl

import scala.meta._

class free extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val imports = scala.collection.immutable.Seq(q"import scalaz._")
    FreeImpl.apply(defn, "scalaz", imports)
  }
}

package freasymonad.cats

import freasymonad.FreeImpl
import scala.meta._

class free extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val imports =
      q"""
        import cats._
        import cats.free._
        import scala.language.higherKinds
      """.stats

    FreeImpl.apply(defn, "cats", imports)
  }
}

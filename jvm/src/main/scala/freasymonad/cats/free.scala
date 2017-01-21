package freasymonad.cats

import freasymonad.FreeImpl

import scala.meta._

class free extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    FreeImpl.apply(defn, freasymonad.libs.Cats)
  }
}

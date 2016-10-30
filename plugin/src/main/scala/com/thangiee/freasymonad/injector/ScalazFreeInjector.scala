package com.thangiee.freasymonad.injector

class ScalazFreeInjector extends FreeInjector {
  val annotationName: String = "freasymonad.scalaz.free"
  val imports: String = "import scalaz._"
  val Inject: String = "scalaz.Inject"
}

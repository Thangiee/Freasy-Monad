package com.thangiee.freasymonad.injector

class CatsFreeInjector extends FreeInjector {
  val annotationName: String = "freasymonad.cats.free"
  val imports: String = "import cats._"
  val Inject: String = "cats.free.Inject"
  val runImplicitParams: String = "implicit m: Monad[M], r: RecursiveTailRecM[M]"
}

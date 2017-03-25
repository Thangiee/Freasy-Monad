0.6.0-SNAPSHOT (2017/03/25)
------------------
* Switch to new macros -- [scala.meta](http://scalameta.org/)
* Don't require installing an IntelliJ plugin to use this library.
* Update Cats to `0.9.0` and Scalaz to `7.2.10` 
* Fix [#9](https://github.com/Thangiee/Freasy-Monad/issues/9)
* **BREAKING CHANGE!** `Interp` trait extends the natural transformation. 
[See change](#https://github.com/Thangiee/Freasy-Monad/commit/a2ef4a1d81b3006ea2d43c993160ec1b2297bde7#diff-ec6e3fef806ba7c082d45e42023ef799L246) --
[example](https://github.com/Thangiee/Freasy-Monad/commit/1db8e50ab171e5c9db9db2bf37a6ac6d83eaae05#diff-1abc6912ae87bf6a2fdf2b98ed7ae4e9L79) 

0.5.0 (2016/11/06)
------------------
* Add support for Scala 2.12 and Scala.js.
* Update to Cats version `0.8.0`.
* Fix plugin issues for older Intellij versions (
  [#3](https://github.com/Thangiee/Freasy-Monad/issues/3) and
  [#5](https://github.com/Thangiee/Freasy-Monad/issues/5)
)

Starting with version `0.5.0`, this project is now published to Sonatype. In addition, group id has been changed to `com.github.thangiee`.
For example, to update from version `0.4.1` to `0.5.0`, change your build.sbt from this:
```scala
resolvers += Resolver.jcenterRepo
libraryDependencies ++= Seq(
  "com.thangiee" %% "freasy-monad" % "0.4.1",
  "org.typelevel" %% "cats" % "0.7.2"
)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```
to the following:
```scala
libraryDependencies ++= Seq(
  "com.github.thangiee" %% "freasy-monad" % "0.5.0",
  "org.typelevel" %% "cats" % "0.8.1" 
)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

0.4.1 (2016/10/10)
------------------
* [#4](https://github.com/Thangiee/Freasy-Monad/issues/4): 
  Fix expanded code does not add type param F to method's rhs in Injects class in some cases.
* [#4](https://github.com/Thangiee/Freasy-Monad/issues/4): 
  Fix: empty-parentheses cause "too many arguments" error for methods in Injects class.

0.4.0 (2016/09/29)
------------------
* Add support for scalaz. (`freasymonad.scalaz.free`)
* **BREAKING CHANGE!** Rename `freasymonad.free` annotation to `freasymonad.cats.free`.
* **BREAKING CHANGE!** Rename `Inject` class to `Injects`.
* Update plugin 

0.3.0 (2016/09/26)
------------------
* [#2](https://github.com/Thangiee/Freasy-Monad/issues/2): Support defining `val`
* Add constraints that result in compiler error. 
* Optimize generated code.
* Update plugin 

0.2.0 (2016/09/10)
------------------
* Replace `Free.liftF` with `Free.inject`.
* Support multiple parameters lists for abstract method.  
* Generate additional `injectOps` obj. and `Inject` class for composing Free monads ADTs. See example [**here.**](https://github.com/Thangiee/Freasy-Monad/blob/master/core/src/test/scala/examples/ComposeFreeMonads.scala) 
* **BREAKING CHANGE!** Move ADT generation outside of `ops` obj. e.g. `KVStore.ops.GrammarADT` to `KVStore.GrammarADT`.
* Plugin fix: error marks when @free trait is defined at root (i.e. not under a package).

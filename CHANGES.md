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

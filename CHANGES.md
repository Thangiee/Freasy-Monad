0.2.0 (2016/09/10)
------------------
* Replace `Free.liftF` with `Free.inject`.
* Support multiple parameters lists for abstract method.  
* Generate additional `injectOps` obj. and `Inject` class for composing Free monads ADTs. See example [**here.**](https://github.com/Thangiee/Freasy-Monad/blob/master/core/src/test/scala/examples/ComposeFreeMonads.scala) 
* **BREAKING CHANGE!** Move ADT generation outside of `ops` obj. e.g. `KVStore.ops.GrammarADT` to `KVStore.GrammarADT`.
* Plugin fix: error marks when @free trait is defined at root (i.e. not under a package).
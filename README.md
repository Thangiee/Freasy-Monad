# Freasy Monad
Freasy Monad library makes it **easy** to create **Free Monad** for [typelevel/cats](https://github.com/typelevel/cats)
and [scalaz/scalaz](https://github.com/scalaz/scalaz). 

If you aren't afraid of bleeding edge, consider using the [meta branch](https://github.com/Thangiee/Freasy-Monad/tree/meta)
which uses the new [scala.meta](http://scalameta.org/) macros. No IntelliJ plugin installation is require with this branch. 

## Getting started

**Important** 
* Version 0.6.0 uses the new [scala.meta](http://scalameta.org/). If using IntelliJ, please uninstall the 
Freasy Monad Plugin if you have it installed. 

* Replace `addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)` with
  `addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full)` for those 
  coming from versions before 0.6.0.

* IntelliJ syntax highlighting only works with 2.11 at the moment due to a known issue: 
  ```
  Mikhail Mutcianko @mutcianm Mar 20 10:17
  @Thangiee @CremboC I see. The problem is caused by the IntelliJ converter that converts trees 
  using 2.11 clasloader(since scala plugin is built with 2.11.8), and they cannot simply be fed 
  into an annotation which runs in 2.12 classloader. This is a major issue and the fix for it 
  might not be trivial, but i'm already working on it.
  ```

Freasy Monad is currently available for Scala 2.11 and 2.12, and [Scala.js](http://www.scala-js.org/).

If you are using `cats`, add the following to your build.sbt: 

```scala
libraryDependencies ++= Seq(
  "com.github.thangiee" %% "freasy-monad" % "0.6.0-SNAPSHOT",
  "org.typelevel" %% "cats" % "0.9.0"
)
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full)
```

If you are using `scalaz`, add the following to your build.sbt: 
```scala
libraryDependencies ++= Seq(
  "com.github.thangiee" %% "freasy-monad" % "0.6.0-SNAPSHOT",
  "org.scalaz" %% "scalaz-core" % "7.2.10"
)
addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full)
```

Starting with version `0.5.0`, group ID has been changed from `com.thangiee` to `com.github.thangiee`.

## `@free` macro
Key-value store example from [cats website](http://typelevel.org/cats/datatypes/freemonad.html) using `free` macro:

```scala
  import cats._
  import cats.free._
  import freasymonad.cats.free // or freasymonad.scalaz.free
  import scala.collection.mutable

  @free trait KVStore {                     // you can use any names you like
    type KVStoreF[A] = Free[GrammarADT, A]  // as long as you define a type alias for Free 
    sealed trait GrammarADT[A]              // and a sealed trait.

    // abstract methods are automatically lifted into part of the grammar ADT
    def put[T](key: String, value: T): KVStoreF[Unit]
    def get[T](key: String): KVStoreF[Option[T]]

    def update[T](key: String, f: T => T): KVStoreF[Unit] =
      for {
        vMaybe <- get[T](key)
        _      <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
      } yield ()
  }

  object Main extends App {
    import KVStore.ops._
  
    def program: KVStoreF[Option[Int]] =
      for {
        _ <- put("wild-cats", 2)
        _ <- update[Int]("wild-cats", _ + 12)
        _ <- put("tame-cats", 5)
        n <- get[Int]("wild-cats")
      } yield n
  
    val impureInterpreter = new KVStore.Interp[Id] {
      val kvs = mutable.Map.empty[String, Any]
      def get[T](key: String): Id[Option[T]] = {
        println(s"get($key)")
        kvs.get(key).map(_.asInstanceOf[T])
      }
      def put[T](key: String, value: T): Id[Unit] = {
        println(s"put($key, $value)")
        kvs(key) = value
      }
    }
    
    impureInterpreter.run(program)
  }
```
Above example for scalaz [here](https://github.com/Thangiee/Freasy-Monad/blob/meta/jvm/src/test/scala/examples/scalaz/KVStore.scala).

During compile time, `KVStore` is expanded to something similar to:
```scala
  object KVStore {
    import cats._
    import cats.free._
    import scala.language.higherKinds
    sealed trait GrammarADT[A]
    object GrammarADT {
      case class Put[T](key: String, value: T) extends GrammarADT[Unit]
      case class Get[T](key: String) extends GrammarADT[Option[T]]
    }
    object ops {
      type KVStoreF[A] = Free[GrammarADT, A]
      def put[T](key: String, value: T): KVStoreF[Unit] = injectOps.put[GrammarADT, T](key, value)
      def get[T](key: String): KVStoreF[Option[T]] = injectOps.get[GrammarADT, T](key)
      def update[T](key: String, f: T => T): KVStoreF[Unit] = injectOps.update[GrammarADT, T](key, f) 
    }
    object injectOps {
      def put[F[_], T](key: String, value: T)(implicit I: Inject[GrammarADT, F]): Free[F, Unit] = Free.liftF(I.inj(GrammarADT.Put(key, value)));
      def get[F[_], T](key: String)(implicit I: Inject[GrammarADT, F]): Free[F, Option[T]] = Free.liftF(I.inj(GrammarADT.Get(key)));
      def update[F[_], T](key: String, f: T => T)(implicit I: Inject[GrammarADT, F]): Free[F, Unit] =
        for {
          vMaybe <- get[F, T](key)
          _      <- vMaybe.map(v => put[F, T](key, f(v))).getOrElse(Free.pure(()))
        } yield ()
    }
    class Injects[F[_]](implicit I: Inject[GrammarADT, F]) {
      def put[T](key: String, value: T): Free[F, Unit] = injectOps.put[F, T](key, value);
      def get[T](key: String): Free[F, Option[T]] = injectOps.get[F, T](key);
      def update[T](key: String, f: T => T): Free[F, Unit] = injectOps.update[F, T](key, f)
    }
    object Injects {
      implicit def injectOps[F[_]](implicit I: Inject[GrammarADT, F]): Inject[F] = new Inject[F]()
    }
    trait Interp[M[_]] extends ~>[KVStore.GrammarADT, M] {
      def apply[A](fa: KVStore.GrammarADT[A]): M[A] = fa match {
        case GrammarADT.Put(key, value) => put(key, value)
        case GrammarADT.Get(key) => get(key)
      }
      def run[A](op: KVStore.ops.KVStoreF[A])(implicit m: cats.Monad[M]): M[A] = op.foldMap(this)
      def put[T](key: String, value: T): M[Unit]
      def get[T](key: String): M[Option[T]]
    }
  }
```

### Benefits

* From the expanded version, we can see that the `free` macro takes care most of the tedious parts when it
comes to writing free monad. The macro uses our abstract methods to define the ADT case classes, create smart 
constructors to case classes, and do pattern matching on the ADT. 

* This library also generate `Inject` for composing Free monads ADTs. See example for 
[**cats**](https://github.com/Thangiee/Freasy-Monad/blob/meta/jvm/src/test/scala/examples/cats/ComposeFreeMonads.scala) and
[**scalaz.**](https://github.com/Thangiee/Freasy-Monad/blob/meta/jvm/src/test/scala/examples/scalaz/ComposeFreeMonads.scala)

* Writing an interpreter using Intellij becomes a breeze:

  ![impl](https://cloud.githubusercontent.com/assets/4734933/18320271/c2904ed6-74ee-11e6-9202-bdb3fc3dc8c2.gif)

* No more false error marks when writing interpreter with Intellij! 
  
  ![regular-interp](https://cloud.githubusercontent.com/assets/4734933/18316097/f5de4ff0-74de-11e6-8542-00daa28c04c7.png) 
  ![freasy-monad-interp](https://cloud.githubusercontent.com/assets/4734933/18316104/f9025b4a-74de-11e6-8b4f-8414df117cea.png)

### IntelliJ support

Since switch to [scala.meta](http://scalameta.org/) in version 0.6.0, syntax highlighting & code completion in IntelliJ 
works without needing to install a plugin. Therefore, please uninstall the **Freasy Monad Plugin** if you are 
coming from a previous version. 

### Constraints

There are some constraints on `@free trait`, and if violated, will result in a compiler error.

* Can not define `var`.
* All `val` and `def` need to have explicit return type.
* Abstract `val` and `def` must have return type of the defined type alias. From the example above, this would be `KVStoreF[...]`.
* `private` and `protected` access modifiers are not allow, use package-private instead.

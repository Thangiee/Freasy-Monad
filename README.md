#Freasy Monad
Freasy Monad library makes it **easy** to create **Free Monad** from [typelevel/cats](https://github.com/typelevel/cats).

It also integrate nicely with Intellij through a plugin to provide proper highlighting & code completion. 

##Getting started
Add the following to your build.sbt: 
```scala
resolvers += Resolver.jcenterRepo
libraryDependencies ++= Seq(
  "com.thangiee" %% "freasy-monad" % "0.1.0",
  ""org.typelevel" %% "cats" % "0.7.2" // requires version 0.7.0+ 
)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

##`@free` macro
Key-value store example from [cats website](http://typelevel.org/cats/tut/freemonad.html) using `free` macro:

```scala
  import cats._
  import cats.free.Free
  import freasymonad.free
  import scala.collection.mutable

  @free trait KVStore {                     // you can use any names you like
    type KVStoreF[A] = Free[GrammarADT, A]  // as long as you define a type alias for Free 
    sealed trait GrammarADT[A]              // and a sealed trait.

    // abstract methods are automatically lifted into part of the grammar ADT
    def put[T](key: String, value: T): KVStoreF[Unit]
    def get[T](key: String): KVStoreF[Option[T]]
    def delete(key: String): KVStoreF[Unit]

    def update[T](key: String, f: T => T): KVStoreF[Unit] =
      for {
        vMaybe <- get[T](key)
        _      <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
      } yield ()
  }

  import KVStore.ops._

  def program: KVStoreF[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
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
    def delete(key: String): Id[Unit] = {
      println(s"delete($key)")
      kvs.remove(key)
    }
  }

  impureInterpreter.run(program)
```

During compile time, `KVStore` is expanded to something similar to:
```scala
  trait KVStore {
    type KVStoreF[A] = Free[GrammarADT, A]
    sealed trait GrammarADT[A]
    object GrammarADT {
      case class Put[T](key: String, value: T) extends GrammarADT[Unit]
      case class Get[T](key: String) extends GrammarADT[Option[T]]
      case class Delete(key: String) extends GrammarADT[Unit]
    }
    def put[T](key: String, value: T): KVStoreF[Unit] = Free.liftF[GrammarADT, Unit](GrammarADT.Put[T](key, value))
    def get[T](key: String): KVStoreF[Option[T]] = Free.liftF[GrammarADT, Option[T]](GrammarADT.Get[T](key))
    def delete(key: String): KVStoreF[Unit] = Free.liftF[GrammarADT, Unit](GrammarADT.Delete(key))
    def update[T](key: String, f: T => T): KVStoreF[Unit] = get[T](key).flatMap((vMaybe) => vMaybe.map((v) => put[T](key, f(v))).getOrElse(Free.pure(())).map((_) => ()))
  }
  object KVStore {
    import cats._
    import scala.language.higherKinds
    object ops extends KVStore
    trait Interp[M[_]] {
      import ops._
      val interpreter = new (GrammarADT ~> M) {
        def apply[A](fa: GrammarADT[A]): M[A] = fa match {
          case GrammarADT.Put(key, value) => put(key, value)
          case GrammarADT.Get(key) => get(key)
          case GrammarADT.Delete(key) => delete(key)
        }
      }
      def run[A](op: KVStoreF[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] = op.foldMap(interpreter)
      def put[T](key: String, value: T): M[Unit]
      def get[T](key: String): M[Option[T]]
      def delete(key: String): M[Unit]
    }
  }
```

### Benefits

* From the expanded version, we can see that the `free` macro takes care most of the tedious parts when it
comes to writing free monad. The macro uses our abstract methods to define the ADT case classes, 
create smart constructors to the case classes using liftF, and do pattern matching on all case classes 
in the interpreter. 

* Writing an interpreter using Intellij becomes a breeze:

  ![impl](https://cloud.githubusercontent.com/assets/4734933/18320271/c2904ed6-74ee-11e6-9202-bdb3fc3dc8c2.gif)

* No more false error marks when writing interpreter with Intellij! 
  
  ![regular-interp](https://cloud.githubusercontent.com/assets/4734933/18316097/f5de4ff0-74de-11e6-8542-00daa28c04c7.png) 
  ![freasy-monad-interp](https://cloud.githubusercontent.com/assets/4734933/18316104/f9025b4a-74de-11e6-8b4f-8414df117cea.png)

### IntelliJ support

Intellij users need to install the **Freasy Monad Plugin** to get proper highlighting & code completion.

1) (Coming Soon) Search and Install the plugin from IntelliJ. (settings > plugins > browse repos > search for "Freasy Monad Plugin" > install and restart) 

  --**OR**--

1) Download the plugin from https://github.com/Thangiee/Freasy-Monad/tree/master/plugin/bin

2) Go to settings, Plugins section, then click on install plugin from disc, and choose this plugin. 

### Todo
* Generate `Free.inject` for composing Free monads ADTs.

package compilation

import cats.Id
import cats.free.Free
import freasymonad.free

// Make sure vals and defs are compiled correctly for ops/injectOps/Inject
object ValDefHandling extends App {
  @free trait KVStore {
    type KVStoreF[A] = Free[GrammarADT, A]
    sealed trait GrammarADT[A]

    val someVal: String = "a"
    def someDef(a: Int): Option[Int] = Some(a + 1)

    def get[T](key: String): KVStoreF[Option[T]]
    val deleteAll: KVStoreF[Unit]

    def foo[T](key: String): KVStoreF[Option[T]] =
      for {
        n <- get[T](key)
        _ = someDef(1)
      } yield n

    val someKey: String = "key123"
    def getSomeKey[T]: KVStoreF[Option[T]] = get[T](someKey)
    val getSomeIntKey: KVStoreF[Option[Int]] = get[Int](someKey)
  }

  import KVStore.GrammarADT

  def program1(implicit I: KVStore.Inject[GrammarADT]) = {
    import I._
    someVal
    val a: Free[GrammarADT, Option[Int]] = getSomeKey[Int]
    val b: Free[GrammarADT, Option[Int]] = getSomeIntKey
    val c: Option[Int] = someDef(1)
    val d: Free[GrammarADT, Unit] = deleteAll
    val e: Free[GrammarADT, Option[String]] = foo[String]("")
  }

  object injectOps {
    import KVStore.injectOps._
    someKey
    val a: Free[GrammarADT, Option[Int]] = getSomeKey[GrammarADT, Int]
    val b: Free[GrammarADT, Option[Int]] = getSomeIntKey[GrammarADT]
    val c: Option[Int] = someDef(1)
    val d: Free[GrammarADT, Unit] = deleteAll[GrammarADT]
    val e: Free[GrammarADT, Option[String]] = foo[GrammarADT, String]("")
  }

  import KVStore.ops._

  val a: KVStoreF[Option[Int]] = getSomeKey[Int]
  val b: KVStoreF[Option[Int]] = getSomeIntKey
  val c: Option[Int] = someDef(1)
  val d: KVStoreF[Unit] = deleteAll
  val e: KVStoreF[Option[String]] = foo[String]("")

  assert(someVal == "a")
  assert(someDef(1).contains(2))

  val interp = new KVStore.Interp[Id] {
    def get[T](key: String): Id[Option[T]] = ???
    def deleteAll: Id[Unit] = ???
  }
}


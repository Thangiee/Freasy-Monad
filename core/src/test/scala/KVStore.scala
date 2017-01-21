import cats.Id
import cats.free.Free
import freasymonad.cats.free

@free trait KVStore22 {
  type KVStoreF[A] = Free[GrammarADT, A]
  sealed trait GrammarADT[A]

  def put[T](key: String, value: T): KVStoreF[Unit]
  def get[T](key: String): KVStoreF[Option[T]]
  def delete(key: String): KVStoreF[Unit]
  val deleteAll: KVStoreF[Unit]
}

object Main1 {

  val interp = new KVStore22.Interp[Id] {
    def put[T](key: String, value: T): Id[Unit] = ???

    def get[T](key: String): Id[Option[T]] = ???

    def delete(key: String): Id[Unit] = ???

    def deleteAll: Id[Unit] = ???
  }

  import KVStore22.ops._
  val res = interp.run(get[Int]("ad"))
}

package compilation

import cats.free._
import freasymonad.cats.free

// https://github.com/Thangiee/Freasy-Monad/issues/9
trait JsonReader[A]

@free trait Issue9 {
  type DataStoreF[A] = Free[DataStoreGrammar, A]
  sealed trait DataStoreGrammar[A]

  def get0[T: JsonReader](): List[T] = ???
  def get1[T](t: T)(implicit ev: JsonReader[T]): DataStoreF[List[T]]
  def get2[T: JsonReader](t: T): DataStoreF[List[T]]
}

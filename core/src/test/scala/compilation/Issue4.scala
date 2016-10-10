package compilation

import cats.free.Free
import freasymonad.cats.free

@free trait FilterStore {
  type FilterStoreF[A] = Free[FilterStoreGrammar, A]
  sealed trait FilterStoreGrammar[A]

  def getAll[T]: FilterStoreF[List[T]]

  // fix: expanded code did not add type param F in some cases such as this one.
  def getCount: FilterStoreF[Int] = getAll[Any].map(_.length)
}


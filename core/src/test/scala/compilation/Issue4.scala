package compilation

import cats.free.Free
import freasymonad.cats.free

// https://github.com/Thangiee/Freasy-Monad/issues/4

@free trait FilterStore1 {
  type FilterStoreF[A] = Free[FilterStoreGrammar, A]
  sealed trait FilterStoreGrammar[A]

  def getAll[T]: FilterStoreF[List[T]]

  // fix: expanded code do not add type param F in injectsOps for some cases such as this one.
  def getCount: FilterStoreF[Int] = getAll[Any].map(_.length).map(identity)
}

@free trait FilterStore2 {
  type FilterStoreF[A] = Free[FilterStoreGrammar, A]
  sealed trait FilterStoreGrammar[A]

  // fix: empty-parentheses cause "too many arguments" error for methods in Injects class
  def getAll[T](): FilterStoreF[List[T]]
  def getCount: FilterStoreF[Int] = getAll[Any]().map(_.length)
}

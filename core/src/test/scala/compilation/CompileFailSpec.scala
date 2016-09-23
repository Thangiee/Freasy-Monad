package compilation

import org.scalatest.{FunSuite, Matchers}

class CompileFailSpec extends FunSuite with Matchers {
  test("Compile fail when val do not have explicit return type") {
    """
      |import cats.free._
      |@freasymonad.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  val foo = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compile fail when def do not have explicit return type") {
    """
      |import cats.free._
      |@freasymonad.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  def foo = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compile fail with abstract var") {
    """
      |import cats.free._
      |@freasymonad.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  var foo: Int
      |}
    """.stripMargin shouldNot compile
  }

  test("Compile fail with concrete var") {
    """
      |import cats.free._
      |@freasymonad.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  var foo: Int = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compile fail when abstract val have wrong return type (not KVStoreF in this case)") {
    """
      |import cats.free._
      |@freasymonad.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  val foo: Int
      |}
    """.stripMargin shouldNot compile
  }

  test("Compile fail when abstract def have wrong return type (not KVStoreF in this case)") {
    """
      |import cats.free._
      |@freasymonad.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  def foo: Int
      |}
    """.stripMargin shouldNot compile
  }
}

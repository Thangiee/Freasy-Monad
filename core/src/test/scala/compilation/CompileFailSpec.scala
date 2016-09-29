package compilation

import org.scalatest.{FunSuite, Matchers}

class CompileFailSpec extends FunSuite with Matchers {
  test("Compiler error when val do not have explicit return type") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  val foo = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error when def do not have explicit return type") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  def foo = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error with abstract var") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  var foo: Int
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error with concrete var") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  var foo: Int = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error when abstract val have wrong return type (not KVStoreF in this case)") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  val foo: Int
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error when abstract def have wrong return type (not KVStoreF in this case)") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  def foo: Int
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error with private val") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  private val foo: Int = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error with protected val") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  protected val foo: Int = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error with private def") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  private def foo: Int = 1
      |}
    """.stripMargin shouldNot compile
  }

  test("Compiler error with protected def") {
    """
      |import cats.free._
      |@freasymonad.cats.free trait KVStore {
      |  type KVStoreF[A] = Free[GrammarADT, A]
      |  sealed trait GrammarADT[A]
      |  protected def foo: Int = 1
      |}
    """.stripMargin shouldNot compile
  }
}

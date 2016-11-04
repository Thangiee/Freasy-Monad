package examples.cats

import cats.data.Coproduct
import cats.free.Free
import cats.{Id, ~>}
import freasymonad.cats.free

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

// example based off https://github.com/typelevel/cats/blob/master/docs/src/main/tut/datatypes/freemonad.md#composing-free-monads-adts
object ComposeFreeMonads extends App {

  @free trait Interact {
    type InteractF[A] = Free[Adt, A]
    sealed trait Adt[A]
    def ask(prompt: String): InteractF[String]
    def tell(msg: String): InteractF[Unit]
  }

  @free trait DataSource {
    type DataSourceF[A] = Free[Adt, A]
    sealed trait Adt[A]
    def addCat(a: String): DataSourceF[Unit]
    def getAllCats: DataSourceF[List[String]]
    def addAndGetAllCats(a: String): DataSourceF[List[String]] =
     for {
       _ <- addCat(a)
       c <- getAllCats
     } yield c
  }

  type CatsApp[A] = Coproduct[DataSource.Adt, Interact.Adt, A]

  // program1 and program2 are the same.
  // This library lets you choose which style you like.

  def program1(implicit I: Interact.Injects[CatsApp], D : DataSource.Injects[CatsApp]): Free[CatsApp, Unit] = {
    import I._, D._
    for {
      cat  <- ask("What's the kitty's name?")
      cats <- addAndGetAllCats(cat)
      _    <- tell(cats.toString)
    } yield ()
  }

  val program2: Free[CatsApp, Unit] = {
    import Interact.injectOps._, DataSource.injectOps._
    for {
      cat  <- ask[CatsApp]("What's the kitty's name?")
      cats <- addAndGetAllCats[CatsApp](cat)
      _    <- tell[CatsApp](cats.toString)
    } yield ()
  }

  val consoleCats = new Interact.Interp[Id] {
    def ask(prompt: String): Id[String] = {
      println(prompt)
      StdIn.readLine()
    }
    def tell(msg: String): Id[Unit] = println(msg)
  }

  val inMemoryDatasource = new DataSource.Interp[Id] {
    private[this] val memDataSet = new ListBuffer[String]
    def addCat(a: String): Id[Unit] = memDataSet.append(a)
    def getAllCats: Id[List[String]] = memDataSet.toList
  }

  val interpreter: CatsApp ~> Id = inMemoryDatasource.interpreter or consoleCats.interpreter

  program1.foldMap(interpreter)
  program2.foldMap(interpreter)
}

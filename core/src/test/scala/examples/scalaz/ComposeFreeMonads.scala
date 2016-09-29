package examples.scalaz

import scalaz._
import scalaz.Id.Id
import freasymonad.scalaz.free

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.language.higherKinds

// example based off https://github.com/typelevel/cats/blob/master/docs/src/main/tut/freemonad.md#composing-free-monads-adts
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

  type ScalazApp[A] = Coproduct[DataSource.Adt, Interact.Adt, A]

  // program1 and program2 are the same.
  // This library lets you choose which style you like.

  def program1(implicit I: Interact.Injects[ScalazApp], D : DataSource.Injects[ScalazApp]): Free[ScalazApp, Unit] = {
    import I._, D._
    for {
      cat  <- ask("What's the kitty's name?")
      cats <- addAndGetAllCats(cat)
      _    <- tell(cats.toString)
    } yield ()
  }

  val program2: Free[ScalazApp, Unit] = {
    import Interact.injectOps._, DataSource.injectOps._
    for {
      cat  <- ask[ScalazApp]("What's the kitty's name?")
      cats <- addAndGetAllCats[ScalazApp](cat)
      _    <- tell[ScalazApp](cats.toString)
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

  // scalaz lacks a convenient `or` atm
  // https://github.com/scalaz/scalaz/issues/1222
  implicit class NaturalTransformationOps[F[_], G[_]](val self: F ~> G) extends AnyVal {
    def or[H[_]](g: H ~> G): ({type λ[α] = Coproduct[F, H, α]})#λ ~> G =
      new (({type λ[α] = Coproduct[F, H, α]})#λ ~> G) {
        def apply[A](fa: Coproduct[F, H, A]): G[A] = fa.run.fold(self.apply, g.apply)
      }
  }

  val interpreter = inMemoryDatasource.interpreter or consoleCats.interpreter

  program1.foldMap(interpreter)
  program2.foldMap(interpreter)
}





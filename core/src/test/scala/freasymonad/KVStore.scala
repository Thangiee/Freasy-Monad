package freasymonad

import cats.Id
import cats.free.Free

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

@free trait KVStore {
  type KVStoreF[A] = Free[GrammarADT, A]
  sealed trait GrammarADT[A]

  def put[T](key: String, value: T): KVStoreF[Unit]
  def get[T](key: String): KVStoreF[Option[T]]

  def update[T](key: String, f: T => T): KVStoreF[Unit] =
    for {
      vMaybe <- get[T](key)
      _      <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()
}

object Main extends App {
  import KVStore.all._

  def program: KVStoreF[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
    } yield n

  val idInterpreter = new Interpreter[Id] {
    val kvs = mutable.Map.empty[String, Any]
    def get[T](key: String): Id[Option[T]] = {
      println(s"get($key)")
      kvs.get(key).map(_.asInstanceOf[T])
    }
    def put[T](key: String, value: T): Id[Unit] = {
      println(s"put($key, $value)")
      kvs(key) = value
    }
  }
  val resId: Id[Option[Int]] = idInterpreter.run(program)

  import cats.implicits.catsStdInstancesForFuture
  import scala.concurrent.ExecutionContext.Implicits.global

  val futureInterpreter = new Interpreter[Future] {
    val kvs = mutable.Map.empty[String, Any]
    def get[T](key: String): Future[Option[T]] = Future {
      println(s"get($key)")
      kvs.get(key).map(_.asInstanceOf[T])
    }
    def put[T](key: String, value: T): Future[Unit] = Future {
      println(s"put($key, $value)")
      kvs(key) = value
    }
  }
  val resFuture: Future[Option[Int]] = futureInterpreter.run(program)
  Await.ready(resFuture, Duration.Inf)
}

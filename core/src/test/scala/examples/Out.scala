package examples

import cats._
import cats.free.{Free, Inject}
import freasymonad.free

import scala.collection.mutable

object Out {

  trait KVStore {
  }

  object KVStore {
    import cats._
    import scala.language.higherKinds

    sealed trait GrammarADT[A]
    object GrammarADT {
      case class Put[T](key: String, value: T) extends GrammarADT[Unit]
      case class Get[T](key: String) extends GrammarADT[Option[T]]
      case class Delete(key: String) extends GrammarADT[Unit]
    }

    object ops {
      type KVStoreF[A] = Free[GrammarADT, A]
      def put[T](key: String, value: T): KVStoreF[Unit] = Free.liftF[GrammarADT, Unit](GrammarADT.Put[T](key, value))
      def get[F[_], T](key: String)(implicit I: Inject[GrammarADT, F]): Free[F, Option[T]] = Free.inject[GrammarADT, F](GrammarADT.Get(key))
      def get[T](key: String): KVStoreF[Option[T]] = get[GrammarADT, T](key)
      def delete(key: String): KVStoreF[Unit] = Free.liftF[GrammarADT, Unit](GrammarADT.Delete(key))
      def update[T](key: String, f: T => T): KVStoreF[Unit] = get[T](key).flatMap((vMaybe) => vMaybe.map((v) => put[T](key, f(v))).getOrElse(Free.pure(())).map((_) => ()))
    }

    class InjectOps[F[_]](implicit I: Inject[GrammarADT, F]) {
      def get[T](key: String)(implicit I: Inject[GrammarADT, F]): Free[F, Option[T]] = ops.get[F, T](key)
    }

    object InjectOps {
      implicit def dataSource[F[_]](implicit I: Inject[GrammarADT, F]): InjectOps[F] = new InjectOps[F]
    }

    trait Interp[M[_]] {
      import ops._
      val interpreter = new (GrammarADT ~> M) {
        def apply[A](fa: GrammarADT[A]): M[A] = fa match {
          case GrammarADT.Put(key, value) => put(key, value)
          case GrammarADT.Get(key) => get(key)
          case GrammarADT.Delete(key) => delete(key)
        }
      }
      def run[A](op: KVStoreF[A])(implicit m: Monad[M], r: RecursiveTailRecM[M]): M[A] = op.foldMap(interpreter)
      def put[T](key: String, value: T): M[Unit]
      def get[T](key: String): M[Option[T]]
      def delete(key: String): M[Unit]
    }
  }
}

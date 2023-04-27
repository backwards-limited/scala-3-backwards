package tech.backwards.fp.learn

import scala.Tuple.Zip
import scala.annotation.targetName
import scala.collection.LazyZip3

abstract class Applicative[F[_]: Functor] {
  val functor: Functor[F] =
    Functor[F]

  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] =
    summon[Applicative[F]]

  object syntax {
    extension [A](a: A) {
      def pure[F[_]: Applicative]: F[A] =
        apply[F].pure(a)
    }

    extension [F[_]: Applicative, A, B](ff: F[A => B]) {
      def ap(fa: F[A]): F[B] =
        apply[F].ap(ff)(fa)

      @targetName("`ap`")
      def <*>(fa: F[A]): F[B] =
        ap(fa)
    }
  }

  given Applicative[List] with {
    def pure[A](a: A): List[A] =
      List(a)

    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      ff flatMap fa.map
  }

  given Applicative[Tuple1] with {
    def pure[A](a: A): Tuple1[A] =
      Tuple1(a)

    def ap[A, B](ff: Tuple1[A => B])(fa: Tuple1[A]): Tuple1[B] =
      Tuple1(ff(0)(fa(0)))
  }

  given Applicative[[X] =>> (X, X)] with {
    def pure[A](a: A): (A, A) =
      a -> a

    def ap[A, B](ff: (A => B, A => B))(fa: (A, A)): (B, B) =
      ff(0)(fa(0)) -> ff(1)(fa(1))
  }

  given Applicative[[X] =>> (X, X, X)] with {
    def pure[A](a: A): (A, A, A) =
      (a, a, a)

    def ap[A, B](ff: (A => B, A => B, A => B))(fa: (A, A, A)): (B, B, B) =
      (ff(0)(fa(0)), ff(1)(fa(1)), ff(2)(fa(2)))
  }
}
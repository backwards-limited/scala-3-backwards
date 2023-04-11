package tech.backwards.fp.learn

import scala.annotation.targetName

abstract class Applicative[F[_]: Functor] {
  val functor: Functor[F] =
    Functor[F]

  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

object Applicative extends ApplicativeGivens {
  def apply[F[_]: Applicative]: Applicative[F] =
    summon[Applicative[F]]

  object syntax {
    implicit class Syntax[A](a: A) {
      def pure[F[_]: Applicative]: F[A] =
        apply[F].pure(a)
    }

    object function {
      extension [F[_]: Applicative, A, B](ff: F[A => B]) {
        def ap(fa: F[A]): F[B] =
          apply[F].ap(ff)(fa)

        @targetName("`ap`")
        def <*>(fa: F[A]): F[B] =
          ap(fa)
      }
    }
  }
}

sealed trait ApplicativeGivens {
  given Applicative[List] with {
    def pure[A](a: A): List[A] =
      List(a)

    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      ff flatMap fa.map
  }
}
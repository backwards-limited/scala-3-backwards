package tech.backwards.fp.learn

import scala.annotation.targetName

abstract class Monad[F[_]: Functor] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Monad extends MonadGivens {
  def apply[F[_]: Monad]: Monad[F] =
    summon[Monad[F]]
  
  object syntax {
    extension [A](a: A) {
      def pure[F[_]: Monad]: F[A] =
        apply[F].pure(a)
    }

    extension [F[_]: Monad, A](fa: F[A]) {
      def flatMap[B](f: A => F[B]): F[B] =
        apply[F].flatMap(fa)(f)

      @targetName("bind")
      def >>=[B](f: A => F[B]): F[B] =
        flatMap(f)
    }
    
    object function {
      extension [F[_]: Monad, A, B](f: A => F[B]) {
        def flatMap(fa: F[A]): F[B] =
          apply[F].flatMap(fa)(f)
          
        @targetName("bind")
        def >>=(fa: F[A]): F[B] =
          flatMap(fa)
      }
    }
  }
}

sealed trait MonadGivens {
  given Monad[List] with {
    def pure[A](a: A): List[A] =
      List(a)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
  }
}
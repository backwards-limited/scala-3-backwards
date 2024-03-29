package tech.backwards.fp.learn

import scala.annotation.targetName

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] =
    summon[Functor[F]]

  object syntax {
    extension [F[_]: Functor, A](fa: F[A]) {
      def fmap[B](f: A => B): F[B] =
        apply[F].fmap(fa)(f)

      def map[B](f: A => B): F[B] =
        fmap(f)

      @targetName("map_op")
      infix def `<$>`[B](f: A => B): F[B] =
        fmap(f)

      def as[B](b: => B): F[B] =
        fmap(_ => b)  
    }
    
    object function {
      // Annoyingly F[A] and A => B are essentially both type constructors, so we have to declare separately
      extension [A, B](f: A => B) {
        def fmap[F[_]: Functor](fa: F[A]): F[B] =
          apply[F].fmap(fa)(f)

        def map[F[_]: Functor](fa: F[A]): F[B] =
          fmap(fa)

        @targetName("map_op")
        infix def `<$>`[F[_]: Functor](fa: F[A]): F[B] =
          fmap(fa)
      } 
    }
  }

  given Functor[List] with {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] =
      fa map f
  }

  given Functor[Tuple1] with {
    def fmap[A, B](fa: Tuple1[A])(f: A => B): Tuple1[B] =
      Tuple1(f(fa(0)))
  }

  given Functor[[X] =>> (X, X)] with {
    def fmap[A, B](fa: (A, A))(f: A => B): (B, B) =
      f(fa(0)) -> f(fa(1))
  }

  given Functor[[X] =>> (X, X, X)] with {
    def fmap[A, B](fa: (A, A, A))(f: A => B): (B, B, B) =
      (f(fa(0)), f(fa(1)), f(fa(2)))
  }
}
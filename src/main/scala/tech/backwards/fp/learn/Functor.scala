package tech.backwards.fp.learn

import scala.annotation.targetName

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor extends FunctorGivens {
  def apply[F[_]: Functor]: Functor[F] =
    summon[Functor[F]]

  object syntax {
    extension [F[_]: Functor, A](fa: F[A]) {
      def fmap[B](f: A => B): F[B] =
        apply[F].fmap(fa)(f)

      @targetName("map")
      def `<$>`[B](f: A => B): F[B] =
        fmap(f)
    }
    
    object function {
      extension [F[_]: Functor, A, B] (f: A => B) {
        def fmap(fa: F[A]): F[B] =
          apply[F].fmap(fa)(f)

        @targetName("map")
        def `<$>`(fa: F[A]): F[B] =
          fmap(fa)
      } 
    }
  }
}

sealed trait FunctorGivens {
  given Functor[List] with {
    def fmap[A, B](fa: List[A])(f: A => B): List[B] =
      fa map f
  }
}
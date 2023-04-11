package tech.backwards.fp.learn

abstract class Traversal[F[_]] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

object Traversal extends TraversalGivens {
  def apply[F[_]: Traversal]: Traversal[F] =
    summon[Traversal[F]]

  object syntax {
    extension [F[_]: Traversal, A](fa: F[A]) {
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
        apply[F].traverse(fa)(f)
    }

    extension [F[_]: Traversal, G[_]: Applicative, A](fa: F[G[A]]) {
      def sequence: G[F[A]] =
        apply[F].traverse(fa)(identity)
    }
  }
}

sealed trait TraversalGivens {

}
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
  given Traversal[[X] =>> (X, X)] with {
    def traverse[G[_]: Applicative, A, B](fa: (A, A))(f: A => G[B]): G[(B, B)] = {
      //import tech.backwards.fp.learn.Applicative.syntax.function.*
      import tech.backwards.fp.learn.Applicative.syntax.*

      Applicative[G].functor.fmap(f(fa._1))((x: B) => (y: B) => (x, y)) ap f(fa._2)
    }
  }
}

/*
implicit val traversalTuple2: Traversal[Lambda[X => (X, X)]] =
  new Traversal[Lambda[X => (X, X)]] {
    def traverse[G[_]: Applicative, A, B](fa: (A, A))(f: A => G[B]): G[(B, B)] = {
      import tech.backwards.fp.learn.Applicative.syntax.function._

      Applicative[G].functor.fmap(f(fa._1))((x: B) => (y: B) => (x, y)) ap f(fa._2)
    }
*/
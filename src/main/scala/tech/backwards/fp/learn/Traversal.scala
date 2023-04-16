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

    extension [A](fa: (A, A)) {
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[(B, B)] =
        traversalTuple2.traverse(fa)(f)
    }

    extension [F[_]: Applicative, A](fa: (F[A], F[A])) {
      def sequence: F[(A, A)] = {
        import tech.backwards.fp.learn.Applicative.syntax.*

        Applicative[F].functor.fmap(fa._1)((x: A) => (y: A) => (x, y)).ap(fa._2)
      }
    }

    extension [F[_]: Applicative, A, B](fa: F[(A, B)]) {
      def sequence: (F[A], F[B]) =
        Applicative[F].functor.fmap(fa)(_._1) -> Applicative[F].functor.fmap(fa)(_._2)
    }

    extension [A](fa: (A, A, A)) {
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[(B, B, B)] =
        traversalTuple3.traverse(fa)(f)
    }

    extension [F[_]: Applicative, A](fa: (F[A], F[A], F[A])) {
      def sequence: F[(A, A, A)] = {
        import tech.backwards.fp.learn.Applicative.syntax.*

        Applicative[F].functor.fmap(fa._1)((x: A) => (y: A) => (z: A) => (x, y, z)).ap(fa._2).ap(fa._3)
      }
    }
  }
}

sealed trait TraversalGivens {
  given traversalTuple2: Traversal[[X] =>> (X, X)] with {
    def traverse[G[_]: Applicative, A, B](fa: (A, A))(f: A => G[B]): G[(B, B)] = {
      import tech.backwards.fp.learn.Applicative.syntax.*

      Applicative[G].functor.fmap(f(fa._1))((x: B) => (y: B) => (x, y)) ap f(fa._2)
    }
  }

  given traversalTuple3: Traversal[[X] =>> (X, X, X)] with {
    def traverse[G[_]: Applicative, A, B](fa: (A, A, A))(f: A => G[B]): G[(B, B, B)] = {
      import tech.backwards.fp.learn.Applicative.syntax.*

      Applicative[G].functor.fmap(f(fa._1))((x: B) => (y: B) => (z: B) => (x, y, z)) ap f(fa._2) ap f(fa._3)
    }
  }

  given Traversal[List] with {
    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      import tech.backwards.fp.learn.Applicative.syntax.*

      fa.foldRight(Applicative[G].pure(List.empty[B]))((a, bs) =>
        Applicative[G].functor.fmap(f(a))((b: B) => (bs: List[B]) => b +: bs) ap bs
      )
    }
  }
}
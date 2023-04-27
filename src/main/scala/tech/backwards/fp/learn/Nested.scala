package tech.backwards.fp.learn

final case class Nested[F[_], G[_], A](value: F[G[A]])

object Nested {
  given [F[_]: Functor, G[_]: Functor]: Functor[[A] =>> Nested[F, G, A]] with {
    def fmap[A, B](fa: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
      Nested(Functor[F].fmap(fa.value)(ga => Functor[G].fmap(ga)(a => f(a))))
  }
  
  given [F[_]: Applicative: Functor, G[_]: Applicative: Functor]: Applicative[[A] =>> Nested[F, G, A]] with {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Applicative.syntax.*
    
    def pure[A](a: A): Nested[F, G, A] =
      Nested(a.pure[G].pure[F]) // i.e. Nested(Applicative[F].pure(Applicative[G].pure(a)))

    def ap[A, B](ff: Nested[F, G, A => B])(fa: Nested[F, G, A]): Nested[F, G, B] =
      Nested(ff.value `<$>` ((gab: G[A => B]) => (ga: G[A]) => gab <*> ga) <*> fa.value)
  }
}
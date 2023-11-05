package tech.backwards.fp.learn

final case class DisjunctionT[F[_], L, R](value: F[Disjunction[L, R]])

object DisjunctionT {
  def pure[F[_]: Applicative, L, R](r: R): DisjunctionT[F, L, R] =
    DisjunctionT(Applicative[F].pure(Right[L, R](r)))

  def lift[F[_]: Functor, L, R](fa: F[R]): DisjunctionT[F, L, R] =
    DisjunctionT(Functor[F].fmap(fa)(Right[L, R]))

  given [F[_]: Functor, L]: Functor[[R] =>> DisjunctionT[F, L, R]] with {
    import tech.backwards.fp.learn.Functor.syntax.*
    
    def fmap[A, B](fa: DisjunctionT[F, L, A])(f: A => B): DisjunctionT[F, L, B] =
      DisjunctionT(Functor[F].fmap(fa.value)(_.fmap(f)))
  }
  
  given [F[_]: Functor: Applicative, L]: Applicative[[R] =>> DisjunctionT[F, L, R]] with {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.*
    
    def pure[A](a: A): DisjunctionT[F, L, A] =
      DisjunctionT(Applicative[F].pure(Right[L, A](a)))

    def ap[A, B](ff: DisjunctionT[F, L, A => B])(fa: DisjunctionT[F, L, A]): DisjunctionT[F, L, B] =
      DisjunctionT(
        ff.value `<$>` ((ff: Disjunction[L, A => B]) => (fa: Disjunction[L, A]) => ff <*> fa) <*> fa.value
      )
  }
  
  given [F[_]: Functor: Monad, L]: Monad[[R] =>> DisjunctionT[F, L, R]] with {
    import tech.backwards.fp.learn.Monad.syntax.*
    
    def pure[A](a: A): DisjunctionT[F, L, A] =
      DisjunctionT(Monad[F].pure(Right[L, A](a)))

    def flatMap[A, B](fa: DisjunctionT[F, L, A])(f: A => DisjunctionT[F, L, B]): DisjunctionT[F, L, B] =
      DisjunctionT(
        fa.value.flatMap {
          case Right(a) => f(a).value
          case Left(l) => Monad[F].pure(Left(l))
        }
      )
  }
}

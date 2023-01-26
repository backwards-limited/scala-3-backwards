package tech.backwards.fp.learn

sealed trait Disjunction[+L, +R]

final case class Left[L, R] private(value: L) extends Disjunction[L, R]

object Left {
  def apply[L, R](l: L): Disjunction[L, R] =
    new Left(l)
}

final case class Right[L, R] private(value: R) extends Disjunction[L, R]

object Right {
  def apply[L, R](r: R): Disjunction[L, R] =
    new Right(r)
}

object Disjunction {
  given [L]: Functor[[A] =>> Disjunction[L, A]] with {
    def fmap[A, B](fa: Disjunction[L, A])(f: A => B): Disjunction[L, B] =
      fa match {
        case Left(l) =>
          Left(l)

        case Right(a) =>
          Right(f(a))
      }
  }

  given [L]: Monad[[A] =>> Disjunction[L, A]] with {
    def pure[A](a: A): Disjunction[L, A] =
      Right(a)

    def flatMap[A, B](fa: Disjunction[L, A])(f: A => Disjunction[L, B]): Disjunction[L, B] =
      fa match {
        case Left(l) =>
          Left(l)

        case Right(a) =>
          f(a)
      }
  }
  
  given [L]: Foldable[[A] =>> Disjunction[L, A]] with {
    def foldr[A, B](fa: Disjunction[L, A])(seed: B)(f: (A, B) => B): B =
      fa match {
        case Left(_) =>
          seed

        case Right(a) =>
          f(a, seed)
      }
  }
}
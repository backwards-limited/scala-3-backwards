package tech.backwards.fp.learn.functor

import scala.language.experimental.erasedDefinitions

sealed trait Disjunction[+L, +R]

final case class Left[L, R](value: L) extends Disjunction[L, R]

object Left {
  given functorLeft[L]: Functor[[A] =>> Left[L, A]] with {
    def fmap[A, B](fa: Left[L, A])(f: A => B): Left[L, B] =
      Left(fa.value)
  }
}

final case class Right[L, R](value: R) extends Disjunction[L, R]

object Right {
  given functorRight[L]: Functor[[A] =>> Right[L, A]] with {
    def fmap[A, B](fa: Right[L, A])(f: A => B): Right[L, B] =
      Right(f(fa.value))
  }
}

object Disjunction {
  given [L]: Functor[[A] =>> Disjunction[L, A]] with {
    def fmap[A, B](fa: Disjunction[L, A])(f: A => B): Disjunction[L, B] =
      fa match {
        case l @ Left(_) =>
          Left.functorLeft.fmap(l)(f)

        case r @ Right(_) =>
          Right.functorRight.fmap(r)(f)
      }
  }
}
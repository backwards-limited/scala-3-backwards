package tech.backwards.fp.learn

import scala.language.experimental.erasedDefinitions

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
}
package tech.backwards.fp.learn.functor

/*
TODO - How to migrate Scala 2 (below) to something like:
enum Maybe[+A] {
  case Just[A](value: A) extends Maybe[A]
  case Nothing[A]() extends Maybe[A]
}
*/

sealed trait Maybe[+A]

final case class Just[A](value: A) extends Maybe[A]

object Just {
  given Functor[Just] with {
    def fmap[A, B](fa: Just[A])(f: A => B): Just[B] =
      Just(f(fa.value))
  }
}

final case class Nothing[A]() extends Maybe[A]

object Nothing {
  def apply[A]: Nothing[A] =
    new Nothing[A]()

  given Functor[Nothing] with {
    def fmap[A, B](fa: Nothing[A])(f: A => B): Nothing[B] =
      apply[B]
  }
}

object Maybe {
  given Functor[Maybe] with {
    def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] =
      fa match {
        case n: Nothing[A] =>
          Nothing.given_Functor_Nothing.fmap(n)(f)

        case j: Just[A] =>
          Just.given_Functor_Just.fmap(j)(f)
      }
  }
}
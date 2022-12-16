package tech.backwards.fp.learn.functor

/*
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
  given Functor[Nothing] with {
    def fmap[A, B](fa: Nothing[A])(f: A => B): Nothing[B] =
      apply[B]
  }

  def apply[A]: Nothing[A] =
    new Nothing[A]()
}

object Maybe {
  /*implicit val functorMaybe: Functor[Maybe] =
    new Functor[Maybe] {
      def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = ???
    }*/
}
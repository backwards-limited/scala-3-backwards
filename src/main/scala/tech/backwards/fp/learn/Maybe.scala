package tech.backwards.fp.learn

/*
TODO - How to migrate Scala 2 (below) to something like:
enum Maybe[+A] {
  case Just[A](value: A) extends Maybe[A]
  case Nothing[A]() extends Maybe[A]
}
*/

sealed trait Maybe[+A]

final case class Just[A] private(value: A) extends Maybe[A]

object Just {
  def apply[A](a: A): Maybe[A] =
    new Just(a)
}

final case class Nothing[A] private() extends Maybe[A]

object Nothing {
  def apply[A]: Maybe[A] =
    new Nothing[A]()
}

object Maybe {
  object syntax {
    extension[A] (a: A) {
      def just: Maybe[A] =
        Just(a)
    }  
  }

  given Functor[Maybe] with {
    def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] =
      fa match {
        case Nothing() =>
          Nothing[B]

        case Just(a) =>
          Just(f(a))
      }
  }

  given Monad[Maybe] with {
    def pure[A](a: A): Maybe[A] =
      Just(a)

    def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
      fa match {
        case Nothing() =>
          Nothing[B]

        case Just(a) =>
          f(a)
      }
  }

  given Foldable[Maybe] with {
    def foldr[A, B](fa: Maybe[A])(seed: B)(f: (A, B) => B): B =
      fa match {
        case Nothing() =>
          seed

        case Just(a) =>
          f(a, seed)
      }
  }

  given Applicative[Maybe] with {
    def pure[A](a: A): Maybe[A] =
      Just(a)

    def ap[A, B](ff: Maybe[A => B])(fa: Maybe[A]): Maybe[B] =
      ff match {
        case Just(f) =>
          Functor[Maybe].fmap(fa)(f)

        case _ =>
          Nothing[B]
      }
  }

  given Traversal[Maybe] with {
    def traverse[G[_]: Applicative, A, B](fa: Maybe[A])(f: A => G[B]): G[Maybe[B]] =
      fa match {
        case Nothing() =>
          Applicative[G].pure(Nothing[B])

        case Just(a) =>
          Applicative[G].functor.fmap(f(a))(Just.apply)
      }
  }
}
package tech.backwards.fp.learn

sealed abstract case class IO[A] private(thunk: () => A) {
  def unsafeRunSync(): A =
    thunk()
}

object IO {
  def apply[A](thunk: => A): IO[A] =
    new IO(() => thunk) {}

  given functorIO: Functor[IO] with {
    def fmap[A, B](fa: IO[A])(f: A => B): IO[B] =
      IO(f(fa.unsafeRunSync()))
  }

  given applicativeIO: Applicative[IO] with {
    def pure[A](a: A): IO[A] =
      IO(a)

    def ap[A, B](ff: IO[A => B])(fa: IO[A]): IO[B] =
      functorIO.fmap(fa)(ff.unsafeRunSync())
  }

  given Monad[IO] with {
    def pure[A](a: A): IO[A] =
      applicativeIO.pure(a)

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      f(fa.unsafeRunSync())
  }
}
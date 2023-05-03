package tech.backwards.fp.learn

import scala.util.chaining.*

final case class State[S, A](run: S => (S, A)) {
  def exec(s: S): S =
    run(s)._1

  def eval(s: S): A =
    run(s)._2
}

object State {
  def put[S](s: S): State[S, Unit] =
    State(_ => s -> ())

  def get[S]: State[S, S] =
    State(s => s -> s)

  def modify[S](f: S => S): State[S, Unit] =
    State(s => f(s) -> ())
    
  given [S]: Functor[[A] =>> State[S, A]] with {
    def fmap[A, B](fa: State[S, A])(f: A => B): State[S, B] =
      State(s =>
        fa.run(s).pipe((s, a) =>
          s -> f(a)
        )
      )
  }

  given [S]: Applicative[[A] =>> State[S, A]] with {
    def pure[A](a: A): State[S, A] =
      State(_ -> a)

    def ap[A, B](ff: State[S, A => B])(fa: State[S, A]): State[S, B] =
      State(s =>
        ff.run(s).pipe((s, f) =>
          fa.run(s).pipe((s, a) =>
            s -> f(a)
          )
        )
      )
  }

  given [S]: Monad[[A] =>> State[S, A]] with {
    def pure[A](a: A): State[S, A] =
      State(_ -> a)

    def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      State(s =>
        fa.run(s).pipe((s, a) =>
          f(a).run(s)
        )
      )
  }
}
package tech.backwards.fp.learn

opaque type Id[A] = A

object Id {
  def apply[A](a: A): Id[A] = a
  
  extension [A](id: Id[A]) {
    def value: A = id
  }

  given Functor[Id] with {
    def fmap[A, B](fa: Id[A])(f: A => B): Id[B] =
      Id(f(fa.value))
  }

  given Applicative[Id] with {
    def pure[A](a: A): Id[A] =
      Id(a)

    def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] =
      ff.value(fa)
  }

  given Monad[Id] with {
    def pure[A](a: A): Id[A] =
      Id(a)

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
      f(fa.value)
  }

  given Foldable[Id] with {
    def foldr[A, B](fa: Id[A])(seed: B)(f: (A, B) => B): B =
      f(fa.value, seed)
  }

  given Traversal[Id] with {
    def traverse[G[_]: Applicative, A, B](fa: Id[A])(f: A => G[B]): G[Id[B]] =
      Applicative[G].functor.fmap(f(fa.value))(Id.apply)
  }
}
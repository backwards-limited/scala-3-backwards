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

  given Monad[Id] with {
    def pure[A](a: A): Id[A] =
      Id(a)

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
      f(fa.value)
  }
}
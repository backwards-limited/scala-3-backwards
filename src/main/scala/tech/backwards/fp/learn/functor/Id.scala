package tech.backwards.fp.learn.functor

opaque type Id[A] = A

object Id {
  def apply[A](a: A): Id[A] = a
  
  extension[A](id: Id[A]) {
    def value: A = id
  } 
}
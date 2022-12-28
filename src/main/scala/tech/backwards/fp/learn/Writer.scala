package tech.backwards.fp.learn

final case class Writer[W, A](run: () => (W, A))

object Writer {
  def tell[W](w: W): Writer[W, Unit] =
    Writer(() => w -> ())
    
  given [W]: Functor[[A] =>> Writer[W, A]] with {
    def fmap[A, B](fa: Writer[W, A])(f: A => B): Writer[W, B] =
      fa.run() match {
        case (w, a) => Writer(() => w -> f(a))
      }
  }
}
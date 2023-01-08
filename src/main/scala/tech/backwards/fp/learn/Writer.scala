package tech.backwards.fp.learn

final case class Writer[W, A](run: () => (W, A))

object Writer {
  def tell[W](w: W): Writer[W, Unit] =
    Writer(() => w -> ())

  def tell[W: Monoid]: Writer[W, Unit] =
    Writer(() => Monoid[W].mzero -> ())
    
  given [W]: Functor[[A] =>> Writer[W, A]] with {
    def fmap[A, B](fa: Writer[W, A])(f: A => B): Writer[W, B] =
      fa.run() match {
        case (w, a) => Writer(() => w -> f(a))
      }
  }

  given [W: Monoid]: Monad[[A] =>> Writer[W, A]] with {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monoid.syntax.*

    def pure[A](a: A): Writer[W, A] =
      tell[W].as(a)

    def flatMap[A, B](fa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
      val (w, a)  = fa.run()
      val (w2, b) = f(a).run()

      tell(w |+| w2).as(b)
    }
  }
}
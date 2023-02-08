package tech.backwards.fp.learn

final case class Writer[W, A](run: () => (W, A))

object Writer {
  def apply[W, A](wa: (W, A)): Writer[W, A] =
    Writer(() => wa)

  def writer[W: Monoid]: Writer[W, Unit] =
    Writer(() => Monoid[W].mzero -> ())

  def tell[W](w: W): Writer[W, Unit] =
    Writer(() => w -> ())


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
      writer[W].as(a)

    def flatMap[A, B](fa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
      val (w, a)  = fa.run()
      val (w2, b) = f(a).run()

      tell(w |+| w2).as(b)
    }
  }

  given [W: Monoid]: Applicative[[A] =>> Writer[W, A]] with {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monoid.syntax.*

    def pure[A](a: A): Writer[W, A] =
      writer[W].as(a)

    def ap[A, B](ff: Writer[W, A => B])(fa: Writer[W, A]): Writer[W, B] =
      (ff.run(), fa.run()) match {
        case ((w, f), (w2, a)) => Writer((w |+| w2) -> f(a))
      }
  }
}
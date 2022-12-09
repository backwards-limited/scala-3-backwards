package tech.backwards.fp.learn.monoid

import scala.annotation.targetName

trait Monoid[A] {
  def mzero: A

  def mappend(x: A, y: A): A
}

object Monoid extends MonoidGivens {
  def apply[A: Monoid]: Monoid[A] =
    summon[Monoid[A]]

  object syntax {
    extension[A: Monoid](x: A) {
      @targetName("mappend")
      def |+|(y: A): A =
        summon[Monoid[A]].mappend(x, y)
    }
  }
}

sealed trait MonoidGivens {
  given Monoid[Sum] with {
    lazy val mzero: Sum =
      Sum(0)

    def mappend(x: Sum, y: Sum): Sum =
      Sum(x.value + y.value)
  }

  given Monoid[Product] with {
    lazy val mzero: Product =
      Product(1)

    def mappend(x: Product, y: Product): Product =
      Product(x.value * y.value)
  }
}
package tech.backwards.fp.learn

import scala.annotation.targetName
import tech.backwards.fp.learn.Monoid.syntax.|+|

trait Monoid[A] {
  def mzero: A

  def mappend(x: A, y: A): A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] =
    summon[Monoid[A]]

  object syntax {
    extension [A: Monoid](x: A) {
      @targetName("mappend")
      def |+|(y: A): A =
        summon[Monoid[A]].mappend(x, y)
    }
  }

  given Monoid[String] with {
    lazy val mzero: String =
      ""

    def mappend(x: String, y: String): String =
      x + y
  }

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

  given [A]: Monoid[List[A]] with {
    lazy val mzero: List[A] =
      Nil

    def mappend(xs: List[A], ys: List[A]): List[A] =
      xs ++ ys
  }

  given [A: Monoid, B: Monoid]: Monoid[Pair[A, B]] with {
    lazy val mzero: Pair[A, B] =
      Pair(Monoid[A].mzero, Monoid[B].mzero)

    def mappend(x: Pair[A, B], y: Pair[A, B]): Pair[A, B] =
      (x, y) match {
        case (Pair(xA, xB), Pair(yA, yB)) =>
          Pair(xA |+| yA, xB |+| yB)
      }
  }
}
package tech.backwards.fp.learn

import cats.implicits.*
import tech.backwards.fp.learn.TotalOrder.syntax.less

trait TotalOrder[A] {
  def less(x: A, y: A): Boolean
}

object TotalOrder { self =>
  def less[A: TotalOrder](x: A, y: A): Boolean =
    summon[TotalOrder[A]].less(x, y)

  object syntax {
    extension [A: TotalOrder](x: A) {
      def less(y: A): Boolean =
        self.less(x, y)
    }
  }

  given TotalOrder[Int] with {
    def less(x: Int, y: Int): Boolean =
      x < y
  }

  given TotalOrder[String] with {
    def less(x: String, y: String): Boolean =
      x < y
  }

  given [A: TotalOrder]: TotalOrder[List[A]] with {
    def less(xs: List[A], ys: List[A]): Boolean =
      xs.zip(ys).foldM(false) { case (outcome, (x, y)) =>
        Option.unless(x less y)(outcome)
      } getOrElse true
  }
}
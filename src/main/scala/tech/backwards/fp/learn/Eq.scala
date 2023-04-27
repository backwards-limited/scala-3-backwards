package tech.backwards.fp.learn

import scala.annotation.targetName
import cats.implicits.*
import tech.backwards.fp.learn.Eq.syntax.====

trait Eq[A] {
  def eq(x: A, y: A): Boolean
}

object Eq { self =>
  def eq[A: Eq](x: A, y: A): Boolean =
    summon[Eq[A]].eq(x, y)

  object syntax {
    extension [A: Eq](x: A) {
      @targetName("eq")
      def ====(y: A): Boolean =
        self.eq(x, y)

      @targetName("!eq")
      def !===(y: A): Boolean =
        ! ====(y)
    }
  }

  given Eq[Int] with {
    def eq(x: Int, y: Int): Boolean =
      x == y
  }

  given Eq[String] with {
    def eq(x: String, y: String): Boolean =
      x == y
  }

  given [A: Eq]: Eq[List[A]] with {
    def eq(xs: List[A], ys: List[A]): Boolean =
      (xs.length == ys.length) && xs.zip(ys).foldM(true) { case (outcome, (x, y)) =>
        Option.when(x ==== y)(outcome)
      }.getOrElse(false)
  }
}
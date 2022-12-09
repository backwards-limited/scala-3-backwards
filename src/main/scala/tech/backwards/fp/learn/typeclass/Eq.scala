package tech.backwards.fp.learn.typeclass

import scala.annotation.targetName
import cats.implicits.*

trait Eq[A] {
  def eq(x: A, y: A): Boolean
}

object Eq extends EqGivens { self =>
  def eq[A: Eq](x: A, y: A): Boolean =
    summon[Eq[A]].eq(x, y)

  object syntax {
    extension[A: Eq](x: A) {
      @targetName("eq")
      def ====(y: A): Boolean =
        self.eq(x, y)

      @targetName("!eq")
      def !===(y: A): Boolean =
        ! ====(y)
    }
  }
}

sealed trait EqGivens {
  import tech.backwards.fp.learn.typeclass.Eq.syntax.*

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
package tech.backwards.fp.learn.typeclass

import cats.implicits._

trait Show[A] {
  def show(x: A): String
}

object Show extends ShowGivens {
  extension [A: Show](x: A) {
    def show: String =
      summon[Show[A]].show(x)
  }
}

sealed trait ShowGivens {
  import tech.backwards.fp.learn.typeclass.Show.*

  given Show[Int] with {
    def show(x: Int): String =
      x.toString
  }

  given Show[Double] with {
    def show(x: Double): String =
      x.toString
  }

  given [A: Show]: Show[List[A]] with {
    def show(xs: List[A]): String =
      xs.map(_.show).mkString("[", ", ", "]")
  }
}
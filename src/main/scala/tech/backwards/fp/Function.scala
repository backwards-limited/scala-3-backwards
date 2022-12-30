package tech.backwards.fp

import scala.annotation.targetName

object Function extends Function

trait Function {
  object syntax {
    extension [A](a: A) {
      @targetName("pipe")
      def |>[B](f: A => B): B =
        f(a)
    }
  }
}
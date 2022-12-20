package tech.backwards.io

import scala.util.chaining.scalaUtilChainingOps
import cats.Show
import cats.implicits.toShow

object Console {
  object syntax {
    import scala.Console.*

    extension (s: String) {
      def color: String => String => String =
        s => color => s"$color$s$RESET"

      def text: String => String =
        color(s)

      def black: String =
        text(BLACK)

      def red: String =
        text(RED)

      def green: String =
        text(GREEN)

      def yellow: String =
        text(YELLOW)

      def blue: String =
        text(BLUE)

      def magenta: String =
        text(MAGENTA)

      def cyan: String =
        text(CYAN)

      def white: String =
        text(WHITE)

      def blackBg: String =
        text(BLACK_B)

      def redBg: String =
        text(RED_B)

      def greenBg: String =
        text(GREEN_B)

      def yellowBg: String =
        text(YELLOW_B)

      def blueBg: String =
        text(BLUE_B)

      def magentaBg: String =
        text(MAGENTA_B)

      def cyanBg: String =
        text(CYAN_B)

      def whiteBg: String =
        text(WHITE_B)
    }

    extension [A: Show](a: A) {
      def debug(modify: String => String = identity): A =
        scala.Console.println(modify(a.show)).pipe(_ => a)
    }
  }
}
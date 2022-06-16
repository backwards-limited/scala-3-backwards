package tech.backwards.bookofmonads.ch1

import scala.annotation.targetName
import munit.*

/**
 * Lists
 */
class Ex2Suite extends FunSuite {
  test("List length") {
    def length[A](xs: List[A]) : Int =
      xs match {
        case Nil => 0
        case _ :: xs => 1 + length(xs)
      }

    assertEquals(length(List("a", "b", "c")), 3)
  }

  test("List concatenation") {
    extension[A](xs: List[A])
      @targetName("append")
      def ++++(ys: List[A]): List[A] =
        xs match {
          case Nil => ys
          case x :: xs => x :: (xs ++++ ys)
        }

    assertEquals(
      List("a", "b") ++++ List("c", "d"),
      List("a", "b", "c", "d")
    )
  }

  test("Map over a list") {
    def map[A, B](f: A => B, xs: List[A]): List[B] =
      xs match {
        case x :: xs => f(x) :: map(f, xs)
        case _ => Nil
      }

    assertEquals(
      map((s: String) => s + "*", List("a", "b")),
      List("a*", "b*")
    )

    assertEquals(
      map((s: String) => s + "*", Nil),
      Nil
    )
  }
}
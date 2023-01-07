package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import tech.backwards.fp.learn.Writer.tell
import tech.backwards.io.Console.syntax.*
import org.scalacheck.Prop.*
import org.scalacheck.Test

class WriterMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Writer Monad pure") {
    assertEquals(
      Monad[[A] =>> Writer[String, A]].pure(5).run(),
      (Monoid[String].mzero -> 5).debug(_.yellow)
    )

    assertEquals(
      Monad[[A] =>> Writer[List[String], A]].pure(5).run(),
      (Monoid[List[String]].mzero -> 5).debug(_.cyan)
    )
  }

  property("Writer Monad flatMap") {
    import tech.backwards.fp.learn.Functor.syntax.*

    assertEquals(
      Monad[[A] =>> Writer[String, A]].flatMap(tell[String].as(5))(x => tell[String].as(x + 1)).run(),
      Monoid[String].mzero -> 6
    )
  }

  /*property("Writer Monad flatMap syntax") {
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      Writer(() => "foo" -> 10).flatMap(x => Writer(() => "bar" -> (x + 1))).run(),
      "foobar" -> 11
    )

    assertEquals(
      Writer(() => List("foo") -> 10).flatMap(x => Writer(() => List("bar") -> (x + 1))).run(),
      List("foo", "bar") -> 11
    )
  }*/

  /*property("Writer Monad pure and flatMap syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      5.pure[Writer[String, *]].flatMap(x => tell[String].as(x + 1)).run(),
      Monoid[String].mzero -> 6
    )
  }*/

  /*property("Writer for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    val writer: Writer[List[String], Int] =
      for {
        x <- 1.pure[Writer[List[String], *]]
        _ <- tell(List("one"))
        y <- 2.pure[Writer[List[String], *]]
        _ <- tell(List("two"))
        z <- 3.pure[Writer[List[String], *]]
        _ <- tell(List("three"))
      } yield x + y + z

    assertEquals(
      writer.run(),
      List("one", "two", "three") -> 6
    )
  }*/

  /*property("Writer for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    type Writer[A] = tech.backwards.fp.learn.Writer[List[String], A]

    val writer: Writer[Int] =
      for {
        x <- 1.pure[Writer]
        _ <- tell(List("one"))
        y <- 2.pure[Writer]
        _ <- tell(List("two"))
        z <- 3.pure[Writer]
        _ <- tell(List("three"))
      } yield x + y + z

    assertEquals(
      writer.run(),
      List("one", "two", "three") -> 6
    )
  }*/

  /*property("Writer Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax.*

    forAll((x: Int) =>
      assertEquals(
        Writer(() => "foo" -> x).flatMap(x => Writer(() => "bar" -> (x + 1))).run(),
        ("foobar" -> (x + 1))
      )
    )
  }*/

  /*property("Writer Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function.*

    assertEquals(
      { x: Int => Writer(() => "bar" -> (x + 1)) }.flatMap(Writer(() => "foo" -> 5)).run(),
      "foobar" -> 6
    )
  }*/
}
package tech.backwards.fp.learn.functor

import cats.implicits.toShow
import munit.ScalaCheckSuite
import tech.backwards.fp.learn.functor.Writer.tell
import tech.backwards.io.Console.syntax.*
import org.scalacheck.Prop.*
import org.scalacheck.Test

class WriterFunctorSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Writer Functor fmap") {
    assertEquals(
      Functor[[A] =>> Writer[String, A]].fmap(Writer(() => "foo" -> 10))(_ + 1).run(),
      "foo" -> 11
    )

    assertEquals(
      Functor[[A] =>> Writer[List[String], A]].fmap(Writer(() => List("foo") -> 10))(_ + 1).run(),
      List("foo") -> 11
    )
  }

  property("Writer Functor fmap syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.*

    assertEquals(
      Writer(() => "foo" -> 10).fmap(_ + 1).run(),
      "foo" -> 11
    )
  }

  property("Writer Functor fmap of arbitrary syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.*

    forAll((x: Int) =>
      assertEquals(
        Writer(() => "foo" -> x).fmap(_ + 1).run(),
        "foo" -> (x + 1)
      )
    )
  }

  property("Writer Functor fmap of function syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.function.*

    assertEquals(
      ((x: Int) => x + 1).fmap(Writer(() => "foo" -> 10)).run(),
      "foo" -> 11
    )
  }

  property("Writer Functor fmap - obey identity") {
    import tech.backwards.fp.learn.functor.Functor.syntax.*

    assertEquals(
      Writer(() => "foo" -> 10).fmap(identity).run(),
      "foo" -> 10
    )
  }

  property("Writer tell") {
    assertEquals(
      tell("foo").run(),
      "foo" -> ()
    )

    assertEquals(
      tell(List("foo")).run(),
      List("foo") -> ()
    )
  }

  property("Writer Functor fmap syntax - obey composition") {
    import tech.backwards.fp.learn.functor.Functor.syntax.*

    val f: Int => Int =
      _ + 2

    val g: Int => Int =
      _ * 3

    val writer: Writer[List[String], Int] =
      Writer(() => List("foo") -> 10)

    assertEquals(
      (writer `<$>` f `<$>` g).run().debug(_.yellow),
      writer.fmap(f andThen g).run()
    )
  }
}
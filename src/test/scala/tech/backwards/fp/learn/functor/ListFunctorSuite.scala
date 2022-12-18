package tech.backwards.fp.learn.functor

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Test

class ListFunctorSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("List Functor fmap")(
    assertEquals(
      Functor[List].fmap(List(1, 2, 3))(_ + 1),
      List(2, 3, 4)
    )
  )

  property("List Functor fmap syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.*

    assertEquals(
      List(1, 2, 3).fmap(_ + 1),
      List(2, 3, 4)
    )

    assertEquals(
      List.empty[Int].fmap(_ + 1),
      Nil
    )
  }

  property("List Functor fmap of function syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.function.*

    assertEquals(
      ((x: Int) => x + 1) `<$>` List(1, 2, 3),
      List(2, 3, 4)
    )
  }

  property("List Functor fmap of arbitrary syntax") {
    import tech.backwards.fp.learn.functor.Functor.syntax.*

    forAll((xs: List[Int]) =>
      assertEquals(
        xs.fmap(_ + 1),
        xs.map(_ + 1)
      )
    )
  }

  property("List Functor fmap - obey identity") {
    import tech.backwards.fp.learn.functor.Functor.syntax.*

    assertEquals(
      List(1, 2, 3) fmap identity,
      List(1, 2, 3)
    )
  }

  property("List Functor fmap syntax - obey composition") {
    import tech.backwards.fp.learn.functor.Functor.syntax.*

    val f: Int => Int =
      _ + 2

    val g: Int => Int =
      _ * 3

    assertEquals(
      List(1, 2, 3) fmap f fmap g,
      List(1, 2, 3).fmap(f andThen g)
    )
  }
}
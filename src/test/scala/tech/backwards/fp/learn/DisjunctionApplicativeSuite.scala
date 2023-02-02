package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Test

class DisjunctionApplicativeSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Disjunction Applicative pure")(
    assertEquals(
      Applicative[[A] =>> Disjunction[String, A]].pure(5),
      Right(5)
    )
  )

  property("Disjunction Applicative pure syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*

    assertEquals(
      5.pure[[A] =>> Disjunction[String, A]],
      Right(5)
    )
  }

  property("Disjunction Right syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax.*

    assertEquals(
      5.right[String],
      Right(5)
    )
  }

  property("Disjunction Left syntax") {
    import tech.backwards.fp.learn.Disjunction.syntax.*

    assertEquals(
      "foo".left[Int],
      Left("foo")
    )
  }

  property("Disjunction Right Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Disjunction[String, Int => Int => Int] =
      Functor[[A] =>> Disjunction[String, A]].fmap(Right(5))(add)

    val addSecondPartiallyApplied: Disjunction[String, Int => Int] =
      Applicative[[A] =>> Disjunction[String, A]].ap(addFirstPartiallyApplied)(Right(10))

    val addThirdAndLastPartiallyApplied: Disjunction[String, Int] =
      Applicative[[A] =>> Disjunction[String, A]].ap(addSecondPartiallyApplied)(Right(20))

    assertEquals(
      addThirdAndLastPartiallyApplied,
      Right(35)
    )
  }

  property("Disjunction Right Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function.*
    import tech.backwards.fp.learn.Functor.syntax.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      Right(5).fmap(add).ap(Right(10)).ap(Right(20)),
      Right(35)
    )

    assertEquals(
      Right(5) fmap add ap Right(10) ap Right(20),
      Right(35)
    )

    assertEquals(
      Right(5) `<$>` add <*> Right(10) <*> Right(20),
      Right(35)
    )
  }

  property("Disjunction Left Applicative ap") {
    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Disjunction[String, Int => Int => Int] =
      Functor[[A] =>> Disjunction[String, A]].fmap(Right(5))(add)

    val addSecondPartiallyApplied: Disjunction[String, Int => Int] =
      Applicative[[A] =>> Disjunction[String, A]].ap(addFirstPartiallyApplied)(Left("foo"))

    val addThirdAndLastPartiallyApplied: Disjunction[String, Int] =
      Applicative[[A] =>> Disjunction[String, A]].ap(addSecondPartiallyApplied)(Right(20))

    assertEquals(
      addThirdAndLastPartiallyApplied,
      Left("foo")
    )
  }

  property("Disjunction Left Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function.*
    import tech.backwards.fp.learn.Disjunction.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      5.right[String].fmap(add).ap(Left("foo")).ap(Right(20)),
      Left("foo")
    )

    assertEquals(
      5.right[String] fmap add ap Right(10) ap Left("foo"),
      Left("foo")
    )

    assertEquals(
      5.right[String] `<$>` add <*> Left("foo") <*> Right(20),
      Left("foo")
    )
  }

  property("Disjunction Right Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function.*
    import tech.backwards.fp.learn.Functor.syntax.function.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      add `<$>` Right(5) <*> Right(10) <*> Right(20),
      Right(35)
    )

    assertEquals(
      ((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Right(5) <*> Right(10) <*> Right(20),
      Right(35)
    )

    assertEquals(
      ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Right(5) <*> Right(10) <*> Right(20),
      Right(35)
    )
  }

  property("Disjunction Left Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function.*
    import tech.backwards.fp.learn.Disjunction.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.function.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      add `<$>` 5.right[String] <*> Left("foo") <*> Right(20),
      Left("foo")
    )

    assertEquals(
      ((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` 5.right[String] <*> Right(10) <*> Left("foo"),
      Left("foo")
    )

    assertEquals(
      ((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` 5.right[String] <*> Right(10) <*> Left("foo"),
      Left("foo")
    )
  }

  property("Disjunction Right Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.function.*
    import tech.backwards.fp.learn.Functor.syntax.function.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        add `<$>` Right(x) <*> Right(y) <*> Right(z),
        Right(x + y + z)
      )
    )
  }
}
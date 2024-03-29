package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import tech.backwards.fp.learn.Writer.*
import org.scalacheck.Prop.*
import org.scalacheck.Test

class WriterApplicativeSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Writer Applicative pure") {
    assertEquals(
      Applicative[[A] =>> Writer[String, A]].pure(5).run(),
      "" -> 5
    )

    assertEquals(
      Applicative[[A] =>> Writer[List[String], A]].pure(5).run(),
      Nil -> 5
    )
  }

  property("Writer Applicative pure syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*

    assertEquals(
      5.pure[[A] =>> Writer[String, A]].run(),
      "" -> 5
    )

    assertEquals(
      5.pure[[A] =>> Writer[List[String], A]].run(),
      Nil -> 5
    )
  }

  property("Writer (String accumulator) Applicative ap") {
    import tech.backwards.fp.learn.Functor.syntax.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Writer[String, Int => Int => Int] =
      Functor[[A] =>> Writer[String, A]].fmap(tell("a").as(5))(add)

    val addSecondPartiallyApplied: Writer[String, Int => Int] =
      Applicative[[A] =>> Writer[String, A]].ap(addFirstPartiallyApplied)(tell("b").as(10))

    val addThirdAndLastPartiallyApplied: Writer[String, Int] =
      Applicative[[A] =>> Writer[String, A]].ap(addSecondPartiallyApplied)(tell("c").as(20))

    assertEquals(
      addThirdAndLastPartiallyApplied.run(),
      "abc" -> 35
    )
  }

  property("Writer (List accumulator) Applicative ap") {
    import tech.backwards.fp.learn.Functor.syntax.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    val addFirstPartiallyApplied: Writer[List[String], Int => Int => Int] =
      Functor[[A] =>> Writer[List[String], A]].fmap(tell(List("a")).as(5))(add)

    val addSecondPartiallyApplied: Writer[List[String], Int => Int] =
      Applicative[[A] =>> Writer[List[String], A]].ap(addFirstPartiallyApplied)(tell(List("b")).as(10))

    val addThirdAndLastPartiallyApplied: Writer[List[String], Int] =
      Applicative[[A] =>> Writer[List[String], A]].ap(addSecondPartiallyApplied)(tell(List("c")).as(20))

    assertEquals(
      addThirdAndLastPartiallyApplied.run(),
      List("a", "b", "c") -> 35
    )
  }

  property("Writer (String accumulator) Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      tell("a").as(5).fmap(add).ap(tell("b").as(10)).ap(tell("c").as(20)).run(),
      "abc" -> 35
    )

    assertEquals(
      (tell("a").as(5) fmap add ap tell("b").as(10) ap tell("c").as(20)).run(),
      "abc" -> 35
    )

    assertEquals(
      (tell("a").as(5) `<$>` add <*> tell("b").as(10) <*> tell("c").as(20)).run(),
      "abc" -> 35
    )
  }

  property("Writer (List accumulator) Applicative ap syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      tell(List("a")).as(5).fmap(add).ap(tell(List("b")).as(10)).ap(tell(List("c")).as(20)).run(),
      List("a", "b", "c") -> 35
    )

    assertEquals(
      (tell(List("a")).as(5) fmap add ap tell(List("b")).as(10) ap tell(List("c")).as(20)).run(),
      List("a", "b", "c") -> 35
    )

    assertEquals(
      (tell(List("a")).as(5) `<$>` add <*> tell(List("b")).as(10) <*> tell(List("c")).as(20)).run(),
      List("a", "b", "c") -> 35
    )
  }

  property("Writer (String accumulator) Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.function.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      (add `<$>` Writer("a" -> 5) <*> Writer("b" -> 10) <*> Writer("c" -> 20)).run(),
      "abc" -> 35
    )

    assertEquals(
      (((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Writer("a" -> 5) <*> Writer("b" -> 10) <*> Writer("c" -> 20)).run(),
      "abc" -> 35
    )

    assertEquals(
      (((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Writer("a" -> 5) <*> Writer("b" -> 10) <*> Writer("c" -> 20)).run(),
      "abc" -> 35
    )
  }

  property("Writer (List accumulator) Applicative ap function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.function.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    assertEquals(
      (add `<$>` Writer(List("a") -> 5) <*> Writer(List("b") -> 10) <*> Writer(List("c") -> 20)).run(),
      List("a", "b", "c") -> 35
    )

    assertEquals(
      (((x: Int) => (y: Int) => (z: Int) => x + y + z) `<$>` Writer(List("a") -> 5) <*> Writer(List("b") -> 10) <*> Writer(List("c") -> 20)).run(),
      List("a", "b", "c") -> 35
    )

    assertEquals(
      (((x: Int, y: Int, z: Int) => x + y + z).curried `<$>` Writer(List("a") -> 5) <*> Writer(List("b") -> 10) <*> Writer(List("c") -> 20)).run(),
      List("a", "b", "c") -> 35
    )
  }

  property("Writer (String accumulator) Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.function.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        (add `<$>` Writer("a" -> x) <*> Writer("b" -> y) <*> Writer("c" -> z)).run(),
        "abc" -> (x + y + z)
      )
    )
  }

  property("Writer (List accumulator) Applicative ap function of arbitrary syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.function.*

    val add: Int => Int => Int => Int =
      x => y => z => x + y + z

    forAll((x: Int, y: Int, z: Int) =>
      assertEquals(
        (add `<$>` Writer(List("a") -> x) <*> Writer(List("b") -> y) <*> Writer(List("c") -> z)).run(),
        List("a", "b", "c") -> (x + y + z)
      )
    )
  }
}
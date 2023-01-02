package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import org.scalacheck.Test

class DisjunctionMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("Disjunction Monad pure")(
    assertEquals(
      Monad[[A] =>> Disjunction[String, A]].pure(5),
      Right(5)
    )
  )

  property("Disjunction Left Monad flatMap") {
    import tech.backwards.fp.Function.syntax.*

    assertEquals(
      Monad[[A] =>> Disjunction[String, A]].flatMap(Left[String, Int]("foo"))(x => Right(x + 1)),
      Left[String, Int]("foo")
    )

    assertEquals(
      Monad[[A] =>> Disjunction[String, A]].flatMap(Left[String, Int]("foo"))(_ + 1 |> Right.apply),
      Left[String, Int]("foo")
    )
  }

  property("Disjunction Left Monad flatMap syntax") {
    import tech.backwards.fp.Function.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      Left[String, Int]("foo").flatMap(x => Right(x + 1)),
      Left[String, Int]("foo")
    )

    assertEquals(
      Left[String, Int]("foo") >>= (_ + 1 |> Right.apply),
      Left[String, Int]("foo")
    )

    assertEquals(
      Right[String, Int](5) >>= (_ => Left[String, Int]("foo")),
      Left[String, Int]("foo")
    )
  }

  property("Disjunction Right Monad flatMap") {
    import tech.backwards.fp.Function.syntax.*

    assertEquals(
      Monad[[A] =>> Disjunction[String, A]].flatMap(Right(5))(x => Right(x + 1)),
      Right(6)
    )

    assertEquals(
      Monad[[A] =>> Disjunction[String, A]].flatMap(Right(5))(_ + 1 |> Right.apply),
      Right(6)
    )
  }

  property("Disjunction Right Monad flatMap syntax") {
    import tech.backwards.fp.Function.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      Right(5).flatMap(x => Right(x + 1)),
      Right(6)
    )

    assertEquals(
      Right(5) >>= (_ + 1 |> Right.apply),
      Right(6)
    )
  }

  property("Disjunction Right Monad pure and flatMap syntax") {
    import tech.backwards.fp.Function.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      5.pure[[A] =>> Disjunction[String, A]].flatMap(x => Right[String, Int](x + 1)),
      6.pure[[A] =>> Disjunction[String, A]]
    )
  }

  property("Disjunction Right type alias Monad pure and flatMap syntax") {
    import tech.backwards.fp.Function.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    type Disjunction[A] = tech.backwards.fp.learn.Disjunction[String, A]

    assertEquals(
      5.pure[Disjunction] >>= (_ + 1 |> Right.apply),
      6.pure[Disjunction]
    )
  }

  property("Disjunction Right Monad flatMap and then map syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      Right(5).flatMap(x => Right(x + 1)).fmap(_ + 1),
      Right(7)
    )

    assertEquals(
      (Right(5) >>= (x => Right(x + 1))) `<$>` (_ + 1),
      Right(7)
    )
  }

  property("Disjunction right for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      for {
        x <- 1.pure[[A] =>> Disjunction[String, A]]
        y <- 2.pure[[A] =>> Disjunction[String, A]]
        z <- 3.pure[[A] =>> Disjunction[String, A]]
      } yield x + y + z,
      Right(6)
    )
  }

  property("Disjunction Left for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    type Disjunction[A] = tech.backwards.fp.learn.Disjunction[String, A]

    assertEquals(
      for {
        x <- 1.pure[Disjunction]
        y <- Left[String, Int]("foo")
        z <- 3.pure[Disjunction]
      } yield x + y + z,
      Left("foo")
    )
  }

  property("Disjunction Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax.*

    forAll((x: Int) =>
      assertEquals(
        Right(x) >>= (x => Right(x + 1)),
        Right(x + 1)
      )
    )
  }

  property("Disjunction Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function.*

    assertEquals(
      ((x: Int) => Right(x + 1)) flatMap Right(5),
      Right(6)
    )

    assertEquals(
      ((x: Int) => Right(x + 1)) >>= Left("foo"),
      Left("foo")
    )
  }
}
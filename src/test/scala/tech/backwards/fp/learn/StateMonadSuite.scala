package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import tech.backwards.io.Console.syntax.*
import org.scalacheck.Prop.*
import org.scalacheck.Test

class StateMonadSuite extends ScalaCheckSuite {
  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("State Monad pure") {
    assertEquals(
      Monad[[A] =>> State[String, A]].pure(5).run("hello"),
      ("hello" -> 5).debug(_.yellow)
    )

    assertEquals(
      Monad[[A] =>> State[List[String], A]].pure(5).run(List("hello")),
      (List("hello") -> 5).debug(_.cyan)
    )
  }

  property("State Monad flatMap") {
    assertEquals(
      Monad[[A] =>> State[String, A]].flatMap(State(_ -> 5))(x => State(_ + " world" -> (x + 1))).run("hello"),
      "hello world" -> 6
    )

    assertEquals(
      Monad[[A] =>> State[List[String], A]].flatMap(State(_ -> 5))(x => State(_ ++ List("world") -> (x + 1))).run(List("hello")),
      List("hello", "world") -> 6
    )
  }

  property("State Monad flatMap syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      State.put("foo").as(10).flatMap(x => State((s: String) => (s + "bar") -> (x + 1))).run("ignore"),
      "foobar" -> 11
    )

    assertEquals(
      (State.put("foo").as(10) >>= (x => State((s: String) => (s + "bar") -> (x + 1)))) run "ignore",
      "foobar" -> 11
    )
  }

  property("State Monad pure and flatMap syntax") {
    import tech.backwards.fp.learn.Monad.syntax.*

    assertEquals(
      5.pure[[A] =>> State[String, A]].flatMap(x => State((s: String) => (s + "bar") -> (x + 1))).run("foo"),
      "foobar" -> 6
    )
  }

  property("State for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    val program: State[List[String], Int] =
      for {
        x <- 1.pure[[A] =>> State[List[String], A]]
        sx <- State.get[List[String]]
        y <- 2.pure[[A] =>> State[List[String], A]]
        _ <- State.put[List[String]](sx :+ "foo")
        z <- 3.pure[[A] =>> State[List[String], A]]
        _ <- State.modify[List[String]](_ :+ "bar")
      } yield x + y + z

    assertEquals(
      program run Nil,
      List("foo", "bar") -> 6
    )
  }

  property("State for comprehension syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    type State[A] = tech.backwards.fp.learn.State[List[String], A]

    val program: State[Int] =
      for {
        x <- 1.pure[State]
        sx <- State.get[List[String]]
        y <- 2.pure[State]
        _ <- State.put[List[String]](sx :+ "foo")
        z <- 3.pure[State]
        _ <- State.modify[List[String]](_ :+ "bar")
      } yield x + y + z

    assertEquals(
      program run List(">>"),
      List(">>", "foo", "bar") -> 6
    )
  }

  property("State Monad flatMap of arbitrary syntax") {
    import tech.backwards.fp.learn.Monad.syntax.*

    forAll((x: Int) =>
      assertEquals(
        State((s: String) => s"$s foo" -> x).flatMap(x => State((s: String) => s"$s bar" -> (x + 1))).run(">>"),
        ">> foo bar" -> (x + 1)
      )
    )
  }

  property("State Monad flatMap of function syntax") {
    import tech.backwards.fp.learn.Monad.syntax.function.*

    assertEquals(
      ((x: Int) => State((s: String) => s"$s bar" -> (x + 1))).flatMap(State((s: String) => s"$s foo" -> 5)).run(">>"),
      ">> foo bar" -> 6
    )
  }
}
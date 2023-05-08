package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

/**
 * First of the following IdT suites:
 *  - IdMonadTransformerMaybeSuite
 *  - IdMonadTransformerDisjunctionSuite
 *  - IdMonadTransformerListSuite
 *  - IdMonadTransformerStateSuite
 *  - IdMonadTransformerWriterSuite
 */
class IdMonadTransformerMaybeSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

  property("IdT") {
    val transformer: IdT[Maybe, Int] =
      IdT(Just(Id(10)))

    assertEquals(
      transformer.value,
      Just(Id(10))
    )
  }

  property("IdT pure") {
    val transformer: IdT[Maybe, Int] =
      IdT.pure[Maybe, Int](10)

    assertEquals(
      transformer.value,
      Just(Id(10))
    )
  }

  property("IdT lift") {
    val transformer: IdT[Maybe, Int] =
      IdT.lift(Just(10))

    assertEquals(
      transformer.value,
      Just(Id(10))
    )

    assertEquals(
      IdT.lift(Just(10)).value,
      Just(Id(10))
    )

    assertEquals(
      IdT.lift(Nothing[Int]).value,
      Nothing[Id[Int]]
    )
  }

  property("IdT Functor") {
    val transformer: IdT[Maybe, Int] =
      IdT(Just(Id(10)))

    assertEquals(
      Functor[[A] =>> IdT[Maybe, A]].fmap(transformer)(_ + 1).value,
      Just(Id(11))
    )

    assertEquals(
      Functor[[A] =>> IdT[Maybe, A]].fmap(IdT(Just(Id(10))))(_ + 1).value,
      Just(Id(11))
    )

    assertEquals(
      Functor[[A] =>> IdT[Maybe, A]].fmap(IdT(Nothing[Id[Int]]))(_ + 1).value,
      Nothing[Id[Int]]
    )
  }

  property("IdT Functor syntax") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Maybe.syntax.*

    val transformer: IdT[Maybe, Int] =
      IdT(Id(10).just)

    assertEquals(
      transformer.fmap(_ + 1).value,
      Id(11).just
    )

    assertEquals(
      IdT(Id(10).just).fmap(_ + 1).value,
      Id(11).just
    )

    assertEquals(
      IdT(Id(10).just) fmap (_ + 1),
      IdT(Id(11).just)
    )

    assertEquals(
      IdT(Id(10).just) `<$>` (_ + 1),
      IdT(Id(11).just)
    )

    assertEquals(
      IdT(nothing[Id[Int]]) `<$>` (_ + 1),
      IdT(nothing[Id[Int]])
    )
  }

  property("IdT Monad") {
    val transformer: IdT[Maybe, Int] =
      Monad[[A] =>> IdT[Maybe, A]].pure(10)

    assertEquals(
      Monad[[A] =>> IdT[Maybe, A]].flatMap(transformer)(a => Monad[[A] =>> IdT[Maybe, A]].pure(a + 1)).value,
      Just(Id(11))
    )

    assertEquals(
      Monad[[A] =>> IdT[Maybe, A]].flatMap(Monad[[A] =>> IdT[Maybe, A]].pure(10))(a => Monad[[A] =>> IdT[Maybe, A]].pure(a + 1)).value,
      Just(Id(11))
    )

    assertEquals(
      Monad[[A] =>> IdT[Maybe, A]].flatMap(IdT(Nothing[Id[Int]]))(a => Monad[[A] =>> IdT[Maybe, A]].pure(a + 1)).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      Monad[[A] =>> IdT[Maybe, A]].flatMap(Monad[[A] =>> IdT[Maybe, A]].pure(10))(_ => IdT(Nothing[Id[Int]])).value,
      Nothing[Id[Int]]
    )
  }

  property("IdT Monad syntax") {
    import tech.backwards.fp.learn.Maybe.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    val transformer: IdT[Maybe, Int] =
      10.pure[[A] =>> IdT[Maybe, A]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[[A] =>> IdT[Maybe, A]]).value,
      Id(11).just
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]].flatMap(a => (a + 1).pure[[A] =>> IdT[Maybe, A]]).value,
      Id(11).just
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]] flatMap (a => IdT(Id(a + 1).just)),
      IdT(Id(11).just)
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]] >>= (a => IdT(Id(a + 1).just)),
      IdT(Id(11).just)
    )

    assertEquals(
      IdT(nothing[Id[Int]]) >>= (a => IdT(Id(a + 1).just)),
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]] >>= (_ => IdT(nothing[Id[Int]])),
      IdT(nothing[Id[Int]])
    )
  }

  property("IdT Applicative") {
    val transformerFn: IdT[Maybe, Int => Int] =
      Applicative[[A] =>> IdT[Maybe, A]].pure((x: Int) => x + 1)

    val transformer: IdT[Maybe, Int] =
      Applicative[[A] =>> IdT[Maybe, A]].pure(10)

    assertEquals(
      Applicative[[A] =>> IdT[Maybe, A]].ap(transformerFn)(transformer).value,
      Just(Id(11))
    )

    assertEquals(
      Applicative[[A] =>> IdT[Maybe, A]].ap(Applicative[[A] =>> IdT[Maybe, A]].pure((x: Int) => x + 1))(Applicative[[A] =>> IdT[Maybe, A]].pure(10)).value,
      Just(Id(11))
    )

    assertEquals(
      Applicative[[A] =>> IdT[Maybe, A]].ap(IdT(Nothing[Id[Int => Int]]))(Applicative[[A] =>> IdT[Maybe, A]].pure(10)).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      Applicative[[A] =>> IdT[Maybe, A]].ap(Applicative[[A] =>> IdT[Maybe, A]].pure((x: Int) => x + 1))(IdT(Nothing[Id[Int]])).value,
      Nothing[Id[Int]]
    )
  }

  property("IdT Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Maybe.syntax.*

    val transformerFn: IdT[Maybe, Int => Int] =
      ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]]

    val transformer: IdT[Maybe, Int] =
      10.pure[[A] =>> IdT[Maybe, A]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Id(11).just
    )

    assertEquals(
      ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]].ap(10.pure[[A] =>> IdT[Maybe, A]]).value,
      Id(11).just
    )

    assertEquals(
      ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]].ap(IdT(nothing[Id[Int]])).value,
      nothing[Id[Int]]
    )

    assertEquals(
      IdT(nothing[Id[Int => Int]]).ap(10.pure[[A] =>> IdT[Maybe, A]]).value,
      nothing[Id[Int]]
    )

    assertEquals(
      IdT(nothing[Id[Int => Int]]) ap 10.pure[[A] =>> IdT[Maybe, A]],
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      IdT(nothing[Id[Int => Int]]) <*> 10.pure[[A] =>> IdT[Maybe, A]],
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]].ap(((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]]).value,
      Id(11).just
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]].ap(IdT(nothing[Id[Int => Int]])).value,
      nothing[Id[Int]]
    )

    assertEquals(
      IdT(nothing[Id[Int]]).ap(((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]]).value,
      nothing[Id[Int]]
    )

    assertEquals(
      IdT(nothing[Id[Int]]) ap ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]],
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      IdT(nothing[Id[Int]]) <*> ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]],
      IdT(nothing[Id[Int]])
    )
  }

  property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Maybe.syntax.*

    assertEquals(
      IdT(Id(10).just) `<$>` (x => (y: Int) => x + y) <*> IdT(Id(1).just),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(nothing[Id[Int]]) `<$>` (x => (y: Int) => x + y) <*> IdT(Id(1).just),
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      IdT(Id(10).just) `<$>` (x => (y: Int) => x + y) <*> IdT(nothing[Id[Int]]),
      IdT(nothing[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      IdT(Id(10).just) `<$>` add <*> IdT(nothing[Id[Int]]),
      IdT(nothing[Id[Int]])
    )
  }

  property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.function.*
    import tech.backwards.fp.learn.Maybe.syntax.*

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Id(10).just) <*> IdT(Id(1).just),
      IdT(Id(11).just)
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(nothing[Id[Int]]) <*> IdT(Id(1).just),
      IdT(nothing[Id[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Id(10).just) <*> IdT(nothing[Id[Int]]),
      IdT(nothing[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` IdT(Id(10).just) <*> IdT(Id(1).just),
      IdT(Id(11).just)
    )
  }

  property("IdT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Maybe.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    val transformer: IdT[Maybe, Int] =
      for {
        x <- 10.pure[[A] =>> IdT[Maybe, A]]
        y <- 11.pure[[A] =>> IdT[Maybe, A]]
        z <- 12.pure[[A] =>> IdT[Maybe, A]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Just(Id(33))
    )

    assertEquals(
      for {
        x <- IdT.lift(10.just)
        y <- IdT.lift(11.just)
        z <- IdT.lift(12.just)
      } yield x + y + z,
      IdT(Id(33).just)
    )

    assertEquals(
      for {
        x <- IdT.lift(10.just)
        y <- IdT.lift(nothing[Int])
        z <- IdT.lift(12.just)
      } yield x + y + z,
      IdT(nothing[Id[Int]])
    )
  }
}
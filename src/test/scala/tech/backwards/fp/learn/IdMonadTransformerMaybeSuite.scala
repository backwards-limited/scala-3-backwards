package tech.backwards.fp.learn

import munit.ScalaCheckSuite
import org.scalacheck.Test

class IdMonadTransformerMaybeSuite extends ScalaCheckSuite {
  override protected def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100).withMaxDiscardRatio(10)

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

    val transformer: IdT[Maybe, Int] =
      IdT(Just(Id(10)))

    assertEquals(
      transformer.fmap(_ + 1).value,
      Just(Id(11))
    )

    assertEquals(
      IdT(Just(Id(10))).fmap(_ + 1).value,
      Just(Id(11))
    )

    assertEquals(
      IdT(Just(Id(10))) fmap (_ + 1),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(Just(Id(10))) `<$>` (_ + 1),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) `<$>` (_ + 1),
      IdT(Nothing[Id[Int]])
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
    import tech.backwards.fp.learn.Monad.syntax.*

    val transformer: IdT[Maybe, Int] =
      10.pure[[A] =>> IdT[Maybe, A]]

    assertEquals(
      transformer.flatMap(a => (a + 1).pure[[A] =>> IdT[Maybe, A]]).value,
      Just(Id(11))
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]].flatMap(a => (a + 1).pure[[A] =>> IdT[Maybe, A]]).value,
      Just(Id(11))
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]] flatMap (a => IdT(Just(Id(a + 1)))),
      IdT(Just(Id(11)))
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]] >>= (a => IdT(Just(Id(a + 1)))),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) >>= (a => IdT(Just(Id(a + 1)))),
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]] >>= (_ => IdT(Nothing[Id[Int]])),
      IdT(Nothing[Id[Int]])
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

    val transformerFn: IdT[Maybe, Int => Int] =
      ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]]

    val transformer: IdT[Maybe, Int] =
      10.pure[[A] =>> IdT[Maybe, A]]

    assertEquals(
      transformerFn.ap(transformer).value,
      Just(Id(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]].ap(10.pure[[A] =>> IdT[Maybe, A]]).value,
      Just(Id(11))
    )

    assertEquals(
      ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]].ap(IdT(Nothing[Id[Int]])).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      IdT(Nothing[Id[Int => Int]]).ap(10.pure[[A] =>> IdT[Maybe, A]]).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      IdT(Nothing[Id[Int => Int]]) ap 10.pure[[A] =>> IdT[Maybe, A]],
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      IdT(Nothing[Id[Int => Int]]) <*> 10.pure[[A] =>> IdT[Maybe, A]],
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]].ap(((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]]).value,
      Just(Id(11))
    )

    assertEquals(
      10.pure[[A] =>> IdT[Maybe, A]].ap(IdT(Nothing[Id[Int => Int]])).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      IdT(Nothing[Id[Int]]).ap(((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]]).value,
      Nothing[Id[Int]]
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) ap ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]],
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) <*> ((x: Int) => x + 1).pure[[A] =>> IdT[Maybe, A]],
      IdT(Nothing[Id[Int]])
    )
  }

  /*property("IdT Functor and Applicative syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.*

    assertEquals(
      IdT(Just(Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(Just(Id(1))),
      IdT(Just(Id(11)))
    )

    assertEquals(
      IdT(Nothing[Id[Int]]) `<$>` (x => (y: Int) => x + y) <*> IdT(Just(Id(1))),
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      IdT(Just(Id(10))) `<$>` (x => (y: Int) => x + y) <*> IdT(Nothing[Id[Int]]),
      IdT(Nothing[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      IdT(Just(Id(10))) `<$>` add <*> IdT(Nothing[Id[Int]]),
      IdT(Nothing[Id[Int]])
    )
  }*/

  /*property("IdT Functor and Applicative function syntax") {
    import tech.backwards.fp.learn.Applicative.syntax.*
    import tech.backwards.fp.learn.Functor.syntax.function.*

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Just(Id(10))) <*> IdT(Just(Id(1))),
      IdT(Just(Id(11)))
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Nothing[Id[Int]]) <*> IdT(Just(Id(1))),
      IdT(Nothing[Id[Int]])
    )

    assertEquals(
      ((x: Int) => (y: Int) => x + y) `<$>` IdT(Just(Id(10))) <*> IdT(Nothing[Id[Int]]),
      IdT(Nothing[Id[Int]])
    )

    val add: Int => Int => Int =
      x => y => x + y

    assertEquals(
      add `<$>` IdT(Just(Id(10))) <*> IdT(Just(Id(1))),
      IdT(Just(Id(11)))
    )
  }*/

  /*property("IdT for comprehension") {
    import tech.backwards.fp.learn.Functor.syntax.*
    import tech.backwards.fp.learn.Monad.syntax.*

    val transformer: IdT[Maybe, Int] =
      for {
        x <- 10.pure[IdT[Maybe, *]]
        y <- 11.pure[IdT[Maybe, *]]
        z <- 12.pure[IdT[Maybe, *]]
      } yield x + y + z

    assertEquals(
      transformer.value,
      Just(Id(33))
    )
  }*/
}
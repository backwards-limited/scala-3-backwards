package tech.backwards.typelevel

import munit.FunSuite

/**
 * [[https://blog.rockthejvm.com/scala-types-kinds/ Types, Kinds and Type Constructors]]
 *
 * [[https://blog.rockthejvm.com/scala-3-type-lambdas/ Type Lambdas in Scala 3]]
 */
class TypeLambdaSuite extends FunSuite {
  test("Level 0: Value Types") {
    val aNumber: Int = 42 // The simplest `kind` of types

    assertEquals(aNumber, 42)
  }

  test("Level 1: Generics") {
    class LinkedList[T]

    class Optional[T]

    val aListOfNumbers: LinkedList[Int] =
      new LinkedList[Int]

    val aListOfStrings: LinkedList[String] =
      new LinkedList[String]

    /*
    Notice we need to use a level-0 type as a type argument before we can use these new types.
    The type LinkedList[Int] is a value type (level-0), because it can be attached to a value.
    Because we can only use LinkedList after we pass a level-0 type as argument to it, LinkedList is a higher-level type.

    Look at how we attached the type LinkedList[Int] to the previous value.
    We used the level-1 type LinkedList and we used the level-0 type argument Int to create a new level-0 type.
    Does that sound similar to something else you’ve seen?

    If you think about it, this mechanism looks similar to a function: take a function, pass a value to it, obtain another value.
    Except in this case, we work with types: take a level-1 type, pass a level-0 type argument to it, obtain another level-0 type.

    For this reason, these generic types are also called type constructors, because they can create level-0 types.
    LinkedList itself is a type constructor: takes a value type (e.g. Int) and returns a value type (e.g. LinkedList[Int]).
    */
  }

  test("Level 2 and Beyond: Higher-Kinded Types") {
    /*
    Scala type system allows the definitions of generic types whose type arguments are also generic.
    We call these higher-kinded types - We can call them level-2 types.
    */
    class Functor[F[_]]

    /*
    Much like LinkedList, Functor itself is a type constructor.
    The underscore marks the fact that the type argument F is itself generic (level-1).
    Because this new type takes a level-1 type argument, the Functor example above is a level-2 type.
    In order to use this type and attach it to a value, we need to use a real level-1 type:
    */
    val functorList: Functor[List] =
      new Functor[List]

    val functorOption: Functor[Option] =
      new Functor[Option]

    /*
    Scala is permissive enough to allow even higher-kinded types (in my terminology, level-3 and above) with nested [_] structures:
    */
    class Meta[F[_[_]]] // A level 3 type

    // And they would work in a similar fashion - pass a type of an inferior-kind (this case, level 2) to use it:
    val metaFunctor: Meta[Functor] =
      new Meta[Functor]

    // Now that you know what a type constructor is, we can expand the concept to types which take multiple type arguments, and perhaps of different kinds. E.g.:
    class HashMap[K, V] // HashMap is (by itself) a type constructor taking two level-0 type arguments.
    val anAddressBook = new HashMap[String, String]

    class ComposedFunctor[F[_], G[_]] // ComposedFunctor is (by itself) a type constructor taking two level-1 type arguments.
    val aComposedFunctor = new ComposedFunctor[List, Option]

    class Formatter[F[_], T] // Formatter is (by itself) a type constructor taking a level-1 type argument and a level-0 type argument.
    val aFormatter = new Formatter[List, String]
  }

  /**
   * - Scala types belong to kinds. Think of kinds as types of types.
   * - Plain types like Int, String or your own non-generic classes belong to the value-level kind — the ones you can attach to values.
   * - Generic types like List belong to what I called the level-1 kind — they take plain (level-0) types as type arguments.
   * - Scala allows us to express higher-kinded types — generic types whose type arguments are also generic; we can call this kind the level-2 kind.
   * - Generic types can’t be attached to values on their own; they need the right type arguments (of inferior kinds) in place; for this reason, they’re called type constructors.
   *
   * You can think of List (the generic type itself) as similar to a function, which takes a level-0 type and returns a level-0 type.
   * This `function` from level-0 types to level-0 types represents the kind which List belongs to.
   *
   * In Scala 2, representing this such a type was horrible:
   * {{{
   *  ({ type T[A] = List[A] })#T
   * }}}
   *
   * In Scala 3, it looks much more similar to a function:
   * {{{
   *  [X] =>> List[X]
   * }}}
   *
   * More Scala 3 examples:
   * {{{
   *  [T] =>> Map[String, T]
   *  // is a type which takes a single type argument T and `returns` a Map type with String as key and T as values.
   *
   *  [T, E] =>> Option[T] Either E
   *  // is a type which takes two type arguments and gives you back a concrete Either type with Option[T] and E.
   *
   *  [F[_]] =>> F[Int]
   *  // is a type which takes a type argument which is itself generic (like List) and gives you back that type, typed with Int.
   * }}}
   */
  test("Type lambdas become important as we start to work with higher-kinded types. Consider Monad:") {
    trait Monad[M[_]] {
      def pure[A](a: A): M[A]

      def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
    }

    class MonadEither[T] extends Monad[[E] =>> Either[T, E]]:
      def pure[A](a: A): Either[T, A] = ???

      def flatMap[A, B](m: Either[T, A])(f: A => Either[T, B]): Either[T, B] = ???
  }
}
import cats._
import cats.implicits._

val v1: String Either Option[Int] = // Right(None)
  Traverse[Option].traverse(none[Int])(_ => "a".asLeft[Int])

val v2: Option[String Either Int] = // Some(Left(a))
  Traverse[[A] =>> String Either A].traverse("a".asLeft[Int])(_ => none[Int])
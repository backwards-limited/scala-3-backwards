package tech.backwards.fp.learn

trait TypeClass[A] {
  def foo(a: A): String
}

object TypeClass {
  extension[A: TypeClass](a: A) {
    def foo: String =
      summon[TypeClass[A]].foo(a)
  }

  given TypeClass[Int] with {
    def foo(a: Int): String =
      s"Int: $a"
  }

  given TypeClass[String] with {
    def foo(a: String): String =
      s"String: $a"
  }
}
package tech.backwards.fp.learn.typeclass

trait TypeClass[A] {
  def foo(a: A): String
}

object TypeClass extends TypeClassGivens {
  def apply[A: TypeClass]: TypeClass[A] = implicitly

  extension[A: TypeClass](a: A) {
    def foo: String =
      TypeClass[A].foo(a)
  }
}

trait TypeClassGivens {
  given TypeClass[Int] with {
    def foo(a: Int): String =
      s"Int: $a"
  }

  given TypeClass[String] with {
    def foo(a: String): String =
      s"String: $a"
  }
}
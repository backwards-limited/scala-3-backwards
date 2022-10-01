package tech.backwards.fp.learn.typeclass

trait TypeClass[A] {
  def foo(a: A): String
}

object TypeClass extends TypeClassGivens {
  extension[A: TypeClass](a: A) {
    def foo: String =
      summon[TypeClass[A]].foo(a)
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
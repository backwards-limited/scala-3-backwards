package tech.backwards.fp.learn.monoid

opaque type Sum = Int

object Sum {
  def apply(value: Int): Sum = value

  extension (sum: Sum) {
    def value: Int = sum
  }
}
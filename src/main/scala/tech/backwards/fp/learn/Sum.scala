package tech.backwards.fp.learn

opaque type Sum = Int

object Sum {
  def apply(value: Int): Sum =
    value

  extension(sum: Sum) {
    def value: Int =
      sum
  }
}
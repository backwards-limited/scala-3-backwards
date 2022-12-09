package tech.backwards.fp.learn.monoid

opaque type Product = Int

object Product {
  def apply(value: Int): Product = value
  
  extension (product: Product) {
    def value: Int = product
  }
}
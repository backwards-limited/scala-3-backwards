package tech.backwards.fp.learn

import scala.annotation.tailrec

trait Foldable[F[_]] {
  def foldr[A, B](fa: F[A])(seed: B)(f: (A, B) => B): B
}

object Foldable {
  def apply[F[_]: Foldable]: Foldable[F] =
    summon[Foldable[F]]

  def foldRight[A, B](xs: List[A])(seed: B)(f: (A, B) => B): B =
    xs.foldRight(seed)(f)
    
  object syntax {
    extension [F[_]: Foldable, A](fa: F[A]) {
      def foldr[B](seed: B)(f: (A, B) => B): B =
        apply[F].foldr(fa)(seed)(f)
    }

    extension [A](fa: (A, A)) {
      def foldr[B](seed: B)(f: (A, B) => B): B =
        foldableTuple2.foldr(fa)(seed)(f)
    }

    extension [A](fa: (A, A, A)) {
      def foldr[B](seed: B)(f: (A, B) => B): B =
        foldableTuple3.foldr(fa)(seed)(f)
    }
  }
  
  given Foldable[List] with {
    def foldr[A, B](fa: List[A])(seed: B)(f: (A, B) => B): B =
      fa.foldRight(seed)(f)
  }

  given foldableTuple2: Foldable[[X] =>> (X, X)] with {
    def foldr[A, B](fa: (A, A))(seed: B)(f: (A, B) => B): B =
      foldRight(fa.productIterator.toList.asInstanceOf[List[A]])(seed)(f)
  }

  given foldableTuple3: Foldable[[X] =>> (X, X, X)] with {
    def foldr[A, B](fa: (A, A, A))(seed: B)(f: (A, B) => B): B =
      foldRight(fa.productIterator.toList.asInstanceOf[List[A]])(seed)(f)
  }
}
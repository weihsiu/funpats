package funpats

import simulacrum.typeclass

/**
  * Created by walter
  */
@typeclass
trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], zero: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: F[A], zero: B)(f: (A, B) => B): B
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit MB: Monoid[B]): B = foldLeft(fa, MB.zero)((b, a) => MB.append(b, f(a)))
  def fold[A : Monoid](fa: F[A]): A = foldMap(fa)(identity)
}

object Foldable {
  implicit def listFoldable[_] = new Foldable[List] {
    def foldLeft[A, B](fa: List[A], zero: B)(f: (B, A) => B) = (zero /: fa)(f)
    def foldRight[A, B](fa: List[A], zero: B)(f: (A, B) => B) = (fa :\ zero)(f)
  }
  implicit def treeFoldable[_] = new Foldable[Tree] {
    def foldLeft[A, B](fa: Tree[A], zero: B)(f: (B, A) => B) = fa match {
      case Branch(l, v, r) => foldLeft(r, f(foldLeft(l, zero)(f), v))(f)
      case Leaf(v) => f(zero, v)
    }
    def foldRight[A, B](fa: Tree[A], zero: B)(f: (A, B) => B): B = fa match {
      case Branch(l, v, r) => foldRight(l, f(v, foldRight(r, zero)(f)))(f)
      case Leaf(v) => f(v, zero)
    }
  }
}

object FoldableExamples {
  Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)
  Foldable[Tree].foldLeft(Branch(Leaf(1), 2, Leaf(3)), 0)(_ + _)
  Foldable[Tree].foldLeft(Branch(Branch(Leaf(1), 2, Leaf(3)), 4, Branch(Leaf(5), 6, Leaf(7))), 0)(_ + _)
  Foldable[Tree].foldLeft(Branch(Branch(Leaf(1), 2, Leaf(3)), 4, Branch(Leaf(5), 6, Leaf(7))), List.empty[Int])(_ :+ _)
  Foldable[Tree].foldRight(Branch(Branch(Leaf(1), 2, Leaf(3)), 4, Branch(Leaf(5), 6, Leaf(7))), List.empty[Int])(_ +: _)
}
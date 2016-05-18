package funpats

import simulacrum.typeclass

/**
  * Created by walter
  */
@typeclass
trait Monoid[A] extends Semigroup[A] {
  def zero: A
}

object Monoid {
  implicit val intMonoid = new Monoid[Int] {
    def append(x: Int, y: Int) = Semigroup[Int].append(x, y)
    def zero = 0
  }
  implicit val stringMonoid = new Monoid[String] {
    def append(x: String, y: String) = Semigroup[String].append(x, y)
    def zero = ""
  }
  implicit def optionMonoid[A : Semigroup] = new Monoid[Option[A]] {
    def append(x: Option[A], y: Option[A]) = Semigroup[Option[A]].append(x, y)
    def zero = None
  }
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def append(x: List[A], y: List[A]) = Semigroup[List[A]].append(x, y)
    def zero = List.empty
  }
  implicit def mapMonoid[A, B : Semigroup] = new Monoid[Map[A, B]] {
    def append(x: Map[A, B], y: Map[A, B]) = Semigroup[Map[A, B]].append(x, y)
    def zero = Map.empty
  }
}

object MonoidLaws extends App {
  import Monoid.ops._
  def associativity[A : Monoid](x: A, y: A, z: A) = (x |+| (y |+| z)) == ((x |+| y) |+| z)
  def leftIdentity[A : Monoid](x: A) = (Monoid[A].zero |+| x) == x
  def rightIdentity[A : Monoid](x: A) = (x |+| Monoid[A].zero) == x
  assert(associativity(1, 2, 3))
  assert(associativity("a", "b", "c"))
  assert(associativity(Option(1), Option(2), Option(3)))
  assert(associativity(List("a"), List("b"), List("c")))
  assert(associativity(Map("a" -> 1), Map("a" -> 2), Map("a" -> 3)))
  assert(leftIdentity(123))
  assert(leftIdentity("abc"))
  assert(leftIdentity(Option(1)))
  assert(leftIdentity(List("a")))
  assert(leftIdentity(Map("a" -> 1)))
  assert(rightIdentity(123))
  assert(rightIdentity("abc"))
  assert(rightIdentity(Option(1)))
  assert(rightIdentity(List("a")))
  assert(rightIdentity(Map("a" -> 1)))
}

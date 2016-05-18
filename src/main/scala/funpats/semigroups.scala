package funpats

import simulacrum.{op, typeclass}

/**
  * Created by walter
  */
@typeclass
trait Semigroup[A] {
  @op("|+|") def append(x: A, y: A): A
}

object Semigroup {
  import Semigroup.ops._
  implicit val intSemigroup = new Semigroup[Int] {
    def append(x: Int, y: Int) = x + y
  }
  implicit val stringSemigroup = new Semigroup[String] {
    def append(x: String, y: String) = x + y
  }
  implicit def optionSemigroup[A : Semigroup] = new Semigroup[Option[A]] {
    def append(x: Option[A], y: Option[A]) = (x, y) match {
      case (None, None) => None
      case (Some(_), None) => x
      case (None, Some(_)) => y
      case (Some(x2), Some(y2)) => Some(x2 |+| y2)
    }
  }
  implicit def listSemigroup[A] = new Semigroup[List[A]] {
    def append(x: List[A], y: List[A]) = x ++ y
  }
  implicit def mapSemigroup[A, B : Semigroup] = new Semigroup[Map[A, B]] {
    def append(x: Map[A, B], y: Map[A, B]) = (x /: y) { case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(_ |+| v)) }
  }
}

object SemigroupLaws extends App {
  import Semigroup.ops._
  def associativity[A : Semigroup](x: A, y: A, z: A) = (x |+| (y |+| z)) == ((x |+| y) |+| z)
  assert(associativity(1, 2, 3))
  assert(associativity("a", "b", "c"))
  assert(associativity(Option(1), Option(2), Option(3)))
  assert(associativity(List("a"), List("b"), List("c")))
  assert(associativity(Map("a" -> 1), Map("a" -> 2), Map("a" -> 3)))


  List(1, 2, 3) |+| List(4, 5, 6)
  Map("a" -> 1, "b" -> 2) |+| Map("b" -> 3, "c" -> 4)
  Map("a" -> List(1, 2), "b" -> List(2)) |+| Map("b" -> List(3), "c" -> List(4))

}
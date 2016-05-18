package funpats

/**
  * Created by walter
  */
object Parametricity {
  // 1. no exceptions (bottom)
  // 2. no nulls
  // 3. no side effects
  // 4. no hashCode() / equals() / toString()
  def foo1[A](x: A): A = ???
  def foo2[A](x: A, y: A): A = ???
  def foo3[A](x: A): (A, A, A) = ???
  def foo4[A](xs: List[A]): List[A] = ???
  def foo5[A, B](xs: List[A]): List[B] = ???
  def foo6[A](x: A): Int = ???
}

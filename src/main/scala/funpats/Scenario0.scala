package funpats

/**
  * Created by walter
  */
object Scenario0 {
  def sum0(xs: List[Int]): Int = {
    var s = 0
    for (x <- xs)
      s += x
    s
  }
  def sum1(xs: List[Int]): Int = xs.foldLeft(0)((a, x) => a + x)
  def sum2[A : Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].zero)((a, x) => Semigroup[A].append(a, x))
  def sum3[F[_] : Foldable, A : Monoid](xs: F[A]): A = Foldable[F].foldMap(xs)(identity)
}

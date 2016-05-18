package funpats

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import simulacrum.typeclass

/**
  * Created by walter
  */
@typeclass
trait Applicative[F[_]] extends Functor[F] { self =>
  def pure[A](a: A): F[A]
  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]
  def ap2[A, B, C](fa: F[A], fb: F[B])(ff: F[(A, B) => C]): F[C] = ap(fa)(ap(fb)(map(ff)(f => b => a => f(a, b))))
  override def map[A, B](fa: F[A])(f: A => B) = ap(fa)(pure(f))
  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = ap(fa)(map(fb)(b => f(_, b)))
  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] = ap(fa)(map2(fb, fc)((b, c) => f(_, b, c)))
  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  def sequence[A](fas: List[F[A]]): F[List[A]] = fas match {
    case Nil => pure(Nil)
    case x :: xs => map2(x, sequence(xs))(_ :: _)
  }
  def compose[G[_] : Applicative]: Applicative[Lambda[X => F[G[X]]]] = new Applicative[Lambda[X => F[G[X]]]] {
    def pure[A](a: A): F[G[A]] = self.pure(implicitly[Applicative[G]].pure(a))
    def ap[A, B](fga: F[G[A]])(fgf: F[G[A => B]]): F[G[B]] =
      self.ap(fga)(self.map(fgf)(gf => implicitly[Applicative[G]].ap(_)(gf)))
  }
}

object Applicative {
  implicit val optionApplicative = new Applicative[Option] {
    def pure[A](a: A) = Some(a)
    def ap[A, B](fa: Option[A])(ff: Option[A => B]) = (fa, ff) match {
      case (None, _) | (_, None) => None
      case (Some(a), Some(f)) => Some(f(a))
    }
  }
  implicit val listApplicative = new Applicative[List] {
    def pure[A](a: A) = List(a)
    def ap[A, B](fa: List[A])(ff: List[A => B]) = for {
      a <- fa
      f <- ff
    } yield f(a)
//    def ap[A, B](fa: List[A])(ff: List[A => B]) = (fa zip ff) map { case (a, f) => f(a) }
  }
  implicit val futureApplicative = new Applicative[Future] {
    def pure[A](a: A) = Future.successful(a)
    def ap[A, B](fa: Future[A])(ff: Future[A => B]) = for {
      a <- fa
      f <- ff
    } yield f(a)
  }
  implicit def function1Applicative[X] = new Applicative[X => ?] {
    def pure[A](a: A) = _ => a
    def ap[A, B](fa: X => A)(ff: X => A => B) = x => ff(x)(fa(x))
  }
}

object ApplicativeLaws extends App {
  import Applicative.ops._
  def identity[F[_] : Applicative, A](fa: F[A]) = fa.ap(Applicative[F].pure((x: A) => x)) == fa
  def composition[F[_] : Applicative, A, B, C](fa: F[A], ff: F[A => B], fg: F[B => C]) =
    fa.ap(ff).ap(fg) == fa.ap(ff.ap(fg.ap(implicitly[Applicative[F]].pure((g: B => C) => (f: A => B) => g compose f))))
  def homomorphism[F[_] : Applicative, A, B](a: A, f: A => B) = Applicative[F].pure(a).ap(Applicative[F].pure(f)) == Applicative[F].pure(f(a))
  def interchange[F[_] : Applicative, A, B](a: A, ff: F[A => B]) = Applicative[F].pure(a).ap(ff) == ff.ap(Applicative[F].pure((f: A => B) => f(a)))
  def map[F[_] : Applicative, A, B](fa: F[A], f: A => B) = fa.ap(Applicative[F].pure(f)) == fa.map(f)
  assert(identity(Option("a")))
  assert(identity(List(1, 2, 3)))
  assert(composition[Option, String, Int, Int](Some("abc"), Some((_: String).length), Some((_: Int) + 1)))
  assert(composition[List, Int, String, Int](List(3), List("a" * (_: Int)), List((_: String).length)))
  assert(homomorphism[Option, String, Int]("abc", _.length))
  assert(homomorphism[List, Int, String](3, "a" * _))
  assert(interchange[Option, String, Int]("abc", Some(_.length)))
  assert(interchange[List, Int, String](3, List("a" * _)))
  assert(map[Option, String, Int](Some("abc"), _.length))
  assert(map[List, Int, String](List(1, 2, 3), "a" * _))
}

object ApplicativeExamples {
  Applicative[String => ?].map2(_.length, _.toInt)(_ + _)
}
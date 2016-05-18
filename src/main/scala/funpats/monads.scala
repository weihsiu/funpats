package funpats

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import simulacrum.typeclass

/**
  * Created by walter
  */
@typeclass
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  override def ap[A, B](fa: F[A])(ff: F[A => B]) = flatMap(ff)(map(fa)(_))
  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
}

object Monad {
  implicit val optionMonad = new Monad[Option] {
    def pure[A](a: A) = Some(a)
    override def map[A, B](fa: Option[A])(f: A => B) = fa.map(f)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa.flatMap(f)
  }
  implicit val listMonad = new Monad[List] {
    def pure[A](a: A) = List(a)
    override def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
    def flatMap[A, B](fa: List[A])(f: A => List[B]) = fa.flatMap(f)
  }
  implicit val futureMonad = new Monad[Future] {
    def pure[A](a: A) = Future.successful(a)
    override def map[A, B](fa: Future[A])(f: A => B) = fa.map(f)
    def flatMap[A, B](fa: Future[A])(f: A => Future[B]) = fa.flatMap(f)
  }
  implicit def function1Monad[X] = new Monad[X => ?] {
    def pure[A](a: A) = _ => a
    override def map[A, B](fa: X => A)(f: A => B) = x => f(fa(x))
    def flatMap[A, B](fa: X => A)(f: A => X => B) = x => f(fa(x))(x)
  }
}

object MonadLaws extends App {
  import Monad.ops._
  def associativity[F[_] : Monad, A, B, C](fa: F[A], f: A => F[B], g: B => F[C]) =
    fa.flatMap(f).flatMap(g) == fa.flatMap(f(_).flatMap(g))
  def leftIdentity[F[_] : Monad, A, B](a: A, f: A => F[B]) =
    Monad[F].pure(a).flatMap(f(_)) == f(a)
  def rightIdentity[F[_] : Monad, A](fa: F[A]) =
    fa.flatMap(Monad[F].pure(_)) == fa
  assert(associativity[Option, String, Int, Int](Some("abc"), s => Some(s.length), x => Some(x + 1)))
  assert(associativity[List, String, Int, Int](List("abc"), s => List(s.length), x => List(x + 1)))
  assert(leftIdentity[Option, String, Int]("abc", s => Some(s.length)))
  assert(leftIdentity[List, String, Int]("abc", s => List(s.length)))
  assert(rightIdentity[Option, String](Some("abc")))
  assert(rightIdentity[List, String](List("abc")))
}

object MonadExamples extends App {
  import Monad.ops._
  val p = for {
    x <- (_: String).length
    y <- (_: String).toInt
  } yield x + y
  println(p("123"))
}
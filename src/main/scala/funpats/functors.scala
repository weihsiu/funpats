package funpats

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import simulacrum.typeclass

/**
  * Created by walter
  */
@typeclass
trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  def as[A, B](fa: F[A], b: => B): F[B] = map(fa)(_ => b)
  def void[A](fa: F[A]): F[Unit] = as(fa, ())
  def compose[G[_] : Functor]: Functor[Lambda[X => F[G[X]]]] = new Functor[Lambda[X => F[G[X]]]] {
    def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = self.map(fga)(implicitly[Functor[G]].map(_)(f))
  }
}

object Functor {
  implicit val optionFunctor = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B) = fa.map(f)
  }
  implicit val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
  }
  implicit val futureFunctor = new Functor[Future] {
    def map[A, B](fa: Future[A])(f: A => B) = fa.map(f)
  }
  implicit def function1Functor[X] = new Functor[X => ?] {
    def map[A, B](fa: X => A)(f: A => B) = x => f(fa(x))
  }
}

object FunctorLaws extends App {
  import Functor.ops._
  def identity[F[_] : Functor, A](fa: F[A]) = fa.map(x => x) == fa
  def associativity[F[_] : Functor, A, B, C](fa: F[A], f: A => B, g: B => C) = fa.map(f).map(g) == fa.map(f andThen g)
  assert(identity(Option(1)))
  assert(identity(List(1, 2, 3)))
//  assert(identity(Future.successful(1)))
  assert(associativity(Option("a"), (_: String).length, (_: Int) + 1))
  assert(associativity(List(1, 2, 3), "a" * (_: Int), (_: String).toUpperCase))
//  assert(associativity(Future.successful(1), "a" * (_: Int), (_: String).toUpperCase))
}

object FunctorExamples {
  Functor[String => ?].map(_.length)(_ * 2)
}
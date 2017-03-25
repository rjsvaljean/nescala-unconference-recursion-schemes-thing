package rxvl.nescala.annotatingTrees

import cats.{Applicative, Monad}

trait Traversable[T[_]] {
  def traverse[F[_]: Applicative, A, B](f: => (A => F[B]))(ta: T[A]): F[T[B]]

  def mapM[M[_]: Monad, A, B](f: => (A => M[B]))(ta: T[A]): M[T[B]] =
    traverse[M, A, B](f)(ta)
}

object Traversable {
  def apply[T[_]](implicit traversable: Traversable[T]) = traversable
}
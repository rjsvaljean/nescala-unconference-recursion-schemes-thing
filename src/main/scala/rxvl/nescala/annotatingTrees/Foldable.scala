package rxvl.nescala.annotatingTrees

import cats.Monoid
import cats.instances.int.catsKernelStdGroupForInt

trait Foldable[T[_]] {
  def foldMap[M : Monoid, A](f: A => M)(ta: T[A]): M
  def fold[M : Monoid](tm: T[M]): M = foldMap[M, M](identity)(tm)
}

object Foldable {
  def apply[T[_]: Foldable] = implicitly[Foldable[T]]
  def count[T[_] : Foldable, A] = Foldable[T].foldMap((_: A) => 1) _
}

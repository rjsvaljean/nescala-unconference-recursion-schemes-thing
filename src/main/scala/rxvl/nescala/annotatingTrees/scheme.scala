package rxvl.nescala.annotatingTrees

import cats.Functor

object scheme {

  import utils.{fmap, both}

  def cata[F[_]: Functor, A](alg: F[A] => A): Fix[F] => A =
    alg compose fmap[F, Fix[F], A](cata(alg)) compose unFix[F]

  def para[F[_]: Functor, A](alg: F[(A, Fix[F])] => A): Fix[F] => A = {
    cata[F, (A, Fix[F])](both(alg, fmap[F, (A, Fix[F]), Fix[F]](_._2) andThen Fix[F])) andThen(_._1)
  }
}

object utils {
  def funzip[F[_]: Functor, A, B](fab: F[(A, B)]): (F[A], F[B]) = (
    Functor[F].map(fab)(_._1),
    Functor[F].map(fab)(_._2)
  )

  def fmap[F[_]: Functor, A, B](f: => A => B): F[A] => F[B] = {
    Functor[F].map(_)(f)
  }

  def both[B, C1, C2](f: B => C1, g: B => C2): B => (C1, C2) = {
    (b: B) => (f(b), g(b))
  }


}
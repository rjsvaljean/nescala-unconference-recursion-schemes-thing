package rxvl.nescala.annotatingTrees

import cats.Functor
import cats.instances.vector.catsKernelStdMonoidForVector
import utils.{fmap, funzip}

object Example4 {
  def cataTrace[F[_], A](
    alg: F[A] => A
  )(
    implicit
    functor: Functor[F],
    foldable: Foldable[F]
  ): Fix[F] => Vector[(Fix[F], A)] = {
    def phi: F[(Vector[(Fix[F], A)], Fix[F])] => Vector[(Fix[F], A)] = { in =>
      val (traceF, inputF) = funzip(in)
      val input: Fix[F] = Fix[F](inputF)
      val traceSoFar : Vector[(Fix[F], A)] = foldable.fold(traceF)
      val output = alg(fmap[F, Fix[F], A]({_k => traceSoFar.find(_._1 == _k).get._2})(functor)(inputF))
      traceSoFar :+ (input -> output)
    }
    scheme.para(phi)
  }

  def run = {
    val trace = cataTrace(Example1.evalAlg(Example1.context))
    trace(Expr.example).map(i => (Example2.ppr(i._1), i._2))
  }

}

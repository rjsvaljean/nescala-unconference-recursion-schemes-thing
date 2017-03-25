package rxvl.nescala.annotatingTrees

import cats.instances.option.catsStdInstancesForOption.tuple2
import ExprF.{Const, Var, Add, Mul, IfNeg}

object Example1 {
  type Env = Map[String, Int]

  def evalAlg(env: Env)(expr: ExprF[Option[Int]]): Option[Int] = {
    expr match {
      case Const(i) => Some(i)
      case Var(id) => env.get(id)
      case Add(r, l) => tuple2(r, l).map { case (_r, _l) => _r + _l }
      case Mul(r, l) => tuple2(r, l).map { case (_r, _l) => _r * _l }
      case IfNeg(condition, ifTrue, ifFalse) =>
        condition.flatMap(c => if (c < 0) ifTrue else ifFalse)
    }
  }


  def eval(env: Env): Expr.T => Option[Int] = {
    scheme.cata(evalAlg(env))
  }

  val context = Map("a" -> 1, "b" -> 3)
  def run = eval(context)(Expr.example)
}

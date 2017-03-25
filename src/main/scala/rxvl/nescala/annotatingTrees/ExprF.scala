package rxvl.nescala.annotatingTrees

import cats.{Applicative, Functor, Monoid}

sealed trait ExprF[+R]

object ExprF {
//  sealed trait Expr
//
//  case class Const(i: Int) extends Expr
//  case class Var(id: String) extends Expr
//  case class Add(r: Expr, l: Expr) extends Expr
//  case class Mul(r: Expr, l: Expr) extends Expr
//  case class IfNeg(condition: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr

  case class Const(i: Int) extends ExprF[Nothing]
  case class Var(id: String) extends ExprF[Nothing]
  case class Add[+R](r: R, l: R) extends ExprF[R]
  case class Mul[+R](r: R, l: R) extends ExprF[R]
  case class IfNeg[+R](condition: R, ifTrue: R, ifFalse: R) extends ExprF[R]

  implicit object functor extends Functor[ExprF] {
    def map[A, B](fa: ExprF[A])(f: (A) => B): ExprF[B] = fa match {
      case Const(i) => Const(i)
      case Var(id) => Var(id)
      case Add(r, l) => Add(f(r), f(l))
      case Mul(r, l) => Mul(f(r), f(l))
      case IfNeg(c, ifT, ifF) => IfNeg(f(c), f(ifT), f(ifF))
    }
  }

  implicit object traversable extends Traversable[ExprF] {
    def traverse[F[_] : Applicative, A, B](f: => (A) => F[B])(ta: ExprF[A]): F[ExprF[B]] = {
      ta match {
        case const @ Const(_) => Applicative[F].pure(const)
        case vaar @ Var(id) => Applicative[F].pure(vaar)
        case Add(r, l) =>
          Applicative[F].map2(f(r), f(l))(Add(_, _))
        case Mul(r, l) =>
          Applicative[F].map2(f(r), f(l))(Mul(_, _))
        case IfNeg(c, ifT, ifF) =>
          Applicative[F].map3(f(c), f(ifT), f(ifF))(IfNeg(_, _, _))
      }
    }
  }

  implicit object foldable extends Foldable[ExprF] {
    def foldMap[M: Monoid, A](f: (A) => M)(ta: ExprF[A]): M = {
      ta match {
        case Const(i) => Monoid[M].empty
        case Var(id) => Monoid[M].empty
        case Add(r, l) => Monoid[M].combine(f(r), f(l))
        case Mul(r, l) => Monoid[M].combine(f(r), f(l))
        case IfNeg(c, ifT, ifF) => Monoid[M].combineAll(Seq(f(c), f(ifT), f(ifF)))
      }

    }
  }

}

case class Fix[F[_] : Functor](unFix: F[Fix[F]])

object unFix { def apply[F[_]]: Fix[F] => F[Fix[F]] = _.unFix }


object Expr {
  // The `pattern functor` ExprF represents the structure of type Expr
  // The isomorphism between a data-type and its pattern functor type
  //   is witnessed by the functions Fix and unfix
  type T = Fix[ExprF]

  def IfNeg[R](condition: Expr.T, ifTrue: Expr.T, ifFalse: Expr.T): Expr.T = Fix(ExprF.IfNeg(condition, ifTrue, ifFalse))
  def Mul[R](r: Expr.T, l: Expr.T): Expr.T = Fix(ExprF.Mul(r, l))
  def Add[R](r: Expr.T, l: Expr.T): Expr.T = Fix(ExprF.Add(r, l))
  def Var(id: String): Expr.T = Fix(ExprF.Var(id): ExprF[Fix[ExprF]])
  def Const(i: Int): Expr.T = Fix(ExprF.Const(i): ExprF[Fix[ExprF]])

  val example = { // ( ifIsNegative ( 1 * a ) b + 0 else b + 2 ) * 3
    Mul(
      IfNeg(
        Mul(Const(1), Var("a")),
        Add(Var("b"), Const(0)),
        Add(Var("b"), Const(2))
      ),
      Const(3)
    )
  }

}

package rxvl.nescala.annotatingTrees

object Example2 {
  def pprAlg(exprF: ExprF[String]): String = {
    import ExprF._
    exprF match {
      case Const(i) => i.toString
      case Var(id) => id
      case Add(r, l) => s"( $r + $l )"
      case Mul(r, l) => s"$r * $l"
      case IfNeg(condition, ifTrue, ifFalse) => s"( if ($condition < 0) $ifTrue else $ifFalse )"
    }
  }

  val ppr = scheme.cata(pprAlg)

  def run = ppr(Expr.example)
}

package rxvl.nescala.annotatingTrees

import cats.instances.set.catsKernelStdSemilatticeForSet

object Example3 {

  def freeVarsAlg(exprF: ExprF[Set[String]]): Set[String] = {
    import ExprF._
    exprF match {
      case Var(id) => Set(id)
      case e => Foldable[ExprF].fold(e)
    }
  }
  val freeVars = scheme.cata(freeVarsAlg)

  def run = freeVars(Expr.example)

}

package compiler

sealed trait Exp {
  override def toString = Exp.toString(this)
}

object Exp {
  case class Var(s: String) extends Exp
  case class Lam(b: String, e: Exp) extends Exp
  case class Tuple(es: List[Exp]) extends Exp
  case class App(e1: Exp, e2: Exp) extends Exp
  case class Let(b: String, e1: Exp, e2: Exp) extends Exp
  case class Lit(l : Literal) extends Exp

  def toString(e: Exp) : String = {
    e match {
      case Var(s) => s
      case Lam(b, e) => s"\\$b -> $e"
      case Tuple(es) => {
        val out = es.map(e => e.toString)
                    .mkString(",")
        s"($out)"
      }
      case App(e1, e2) => s"$e1 $e2"
      case Let(b, e1, e2) => s"let $b = $e1 in $e2"
      case Lit(l) => l.toString
    }
  }
}

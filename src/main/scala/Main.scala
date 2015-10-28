import compiler._

object Main {
  def main(args: Array[String]) = {
    println ("Hello World")
    val lit = Literal.CharLit('c')
    println(TypeInfer.typeOf(Exp.Lit(lit)))
  }
}

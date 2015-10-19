package ast

sealed trait Literal {
  override def toString = Literal.toString(this)
}

object Literal {
  case class CharLit(ch: Char) extends Literal
  case class StringLit(s: String) extends Literal
  case class IntegerLit(i: Int) extends Literal
  case class FloatLit(f: Float) extends Literal

  def toString(l : Literal) : String = {
    l match {
      case CharLit(ch) => ch.toString
      case StringLit(s) => s
      case IntegerLit(i) => i.toString
      case FloatLit(f) => f.toString
    }
  }
}

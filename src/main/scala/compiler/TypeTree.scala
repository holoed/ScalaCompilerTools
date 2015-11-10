package compiler

sealed trait Type {
  override def toString = Type.toString(this)
}

object Type {
  case class TyLam(t1: Type, t2: Type) extends Type
  case class TyVar(s: String) extends Type
  case class TyCon(s: String, ts: List[Type]) extends Type

  def toString(t: Type) = {
    t match {
      case TyVar (s) => s
      case TyLam(t1, t2) => s"$t1 -> $t2"
      case TyCon(s, List()) => s
      case TyCon(s, ts) => {
        val out = ts.map(t => t.toString)
                    .mkString(", ")
        s"$s($out)"
      }
    }
  }
}

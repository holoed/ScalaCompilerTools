package compiler

import scala.collection.immutable.Map

sealed case class Subst(subs: Map[String, Type])

object Substitutions {

  def extend(s:String)(t:Type)(sb : Subst) : Subst = sb match {
    case Subst(subs) => Subst(subs + (s -> t))
  }

  def lookup(s:String)(sb:Subst) : Type = sb match {
    case Subst(subs) => subs getOrElse (s, Type.TyVar(s))
  }

  def subs(t:Type)(sb:Subst) : Type = t match {
    case t@Type.TyVar(n) => {
      val t_ = lookup(n)(sb)
      if (t == t_) t_ else subs(t_)(sb)
    }
    case Type.TyLam(a, r) => Type.TyLam(subs (a)(sb), subs (r)(sb))
    case Type.TyCon(name, tyArgs) => Type.TyCon(name, tyArgs map (x => subs(x)(sb)))
  }
}

package substitutions

import types._
import scala.collection.immutable.Map

sealed case class Subst(subs: Map[String, Type])

object Substitutions {

  def extend(s:String)(t:Type)(sb : Subst) : Subst = sb match {
    case Subst(subs) => Subst(subs + (s -> t))
  }

  def lookup(s:String)(sb:Subst) : Type = sb match {
    case Subst(subs) => subs getOrElse (s, Type.TyVar(s))
  }
}

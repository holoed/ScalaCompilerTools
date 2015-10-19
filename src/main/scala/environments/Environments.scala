package environments

import types._
import scala.collection.immutable.Set
import scala.collection.immutable.Map

sealed case class TyScheme(t: Type, vs: Set[String])

sealed case class Env(d: Map[String, TyScheme])

object Environments {
  def getTVarsOfType(t: Type) : Set[String] = {
    t match {
      case Type.TyVar(n) => Set(n)
      case Type.TyLam(t1, t2) => (getTVarsOfType(t1)) | (getTVarsOfType(t2))
      case Type.TyCon(_, args) => args.foldLeft (Set.empty[String]) ((acc,t) => acc | getTVarsOfType(t))
    }
  }
}

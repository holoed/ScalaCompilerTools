package compiler

import scala.collection.immutable.Set
import scala.collection.immutable.Map

sealed case class TyScheme(t: Type, tvars: Set[String])

sealed case class Env(d: Map[String, TyScheme])

object Environments {
  def getTVarsOfType(t: Type) : Set[String] = {
    t match {
      case Type.TyVar(n) => Set(n)
      case Type.TyLam(t1, t2) => (getTVarsOfType(t1)) | (getTVarsOfType(t2))
      case Type.TyCon(_, args) => args.foldLeft (Set.empty[String]) ((acc,t) => acc | getTVarsOfType(t))
    }
  }

  def getTVarsOfScheme(sc: TyScheme) : Set[String] = {
    sc match {
      case TyScheme(t, tvars) => getTVarsOfType(t) -- tvars
    }
  }

  def getTVarsOfEnv(env:Env) : Set[String] = {
    env match {
      case Env(d) => d.values.foldLeft (Set.empty[String]) ((acc, s) => acc | getTVarsOfScheme(s))
    }
  }
}

package typeinference

import ast._
import types._
import environments._
import unifications._
import substitutions._

object TypeInfer {
  def newTyVar(state: Int) : (Int, Type) = {
    val tyVar = Type.TyVar(s"T$state")
    (state + 1, tyVar)
  }

  def integerCon = Type.TyCon("int", List())

  def floatCon = Type.TyCon("float", List())

  def charCon = Type.TyCon("char", List())

  def stringCon = Type.TyCon("string", List())

  def litToTy(lit: Literal) : Type = lit match {
    case Literal.CharLit(_) => charCon
    case Literal.IntegerLit(_) => integerCon
    case Literal.FloatLit(_) => floatCon
    case Literal.StringLit(_) => stringCon
  }

  def findSc(n: String, env: Env) : TyScheme = env match {
    case Env(dict) => dict.get(n).get
  }

  def containsSc(n: String, env: Env) : Boolean = env match {
    case Env(dict) => dict.contains(n)
  }

  def addSc(n: String, sc: TyScheme, env:Env) : Env = env match {
    case Env(dict) => Env(dict + (n -> sc))
  }

  def tp(env: Env, e: Exp, bt: Type, subs:Subst, state:Int) : (Int, Subst) =
    e match {
      case Exp.Lit(v) => (state, Unification.mgu (litToTy(v)) (bt) (subs))
      //TODO: Add other cases for expression.
  }
}

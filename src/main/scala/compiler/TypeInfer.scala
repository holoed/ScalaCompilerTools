package compiler

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

  def findSc(n: String) (env: Env) : TyScheme = env match {
    case Env(dict) => dict.get(n).get
  }

  def containsSc(n: String) (env: Env) : Boolean = env match {
    case Env(dict) => dict.contains(n)
  }

  def addSc(n: String) (sc: TyScheme) (env:Env) : Env = env match {
    case Env(dict) => Env(dict + (n -> sc))
  }

  def tp(env: Env) (e: Exp) (bt: Type) (subs:Subst) (state:Int) : (Int, Subst) =
    e match {
      case Exp.Lit(v) => (state, Unification.mgu (litToTy(v)) (bt) (subs))

      case Exp.Var(n) => (state, if (!containsSc(n)(env)) throw new Exception(s"Cannot find name $n")
                                 else (findSc(n)(env)) match {
                                   case TyScheme(t, _) => Unification.mgu (Substitutions.subs(t)(subs)) (bt) (subs)
                                 })

      case Exp.Lam(b, e) => {
        val (state2, tyVarA) = newTyVar(state)
        val (state3, tyVarB) = newTyVar(state2)
        val subs1 = Unification.mgu (bt) (Type.TyLam(tyVarA, tyVarB)) (subs)
        val newEnv = addSc (b) (TyScheme(tyVarA, Set())) (env)
        tp (newEnv) (e) (tyVarB) (subs1) (state3)
      }
  }

  val predefinedEnv: Env = Env(Map[String, TyScheme]())

  def typeOf(e:Exp):Type = {
    val state = 0
    val (state1, tyVar) = newTyVar(state)
    val subs = Subst(Map[String, Type]())
    val (_, subs1) = tp (predefinedEnv) (e) (tyVar) (subs) (state1)
    Substitutions.subs(tyVar)(subs1)
  }
}

package compiler

object TypeInfer {
  def newTyVar : State[Type, Int] = for {
    state <- State.getState[Int]
    val tyVar = Type.TyVar(s"T$state")
    _ <- State.putState(state + 1)
  } yield tyVar

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

  def tp(env: Env) (e: Exp) (bt: Type) (subs:Subst) : State[Subst, Int] =
    e match {
      case Exp.Lit(v) => State.unit (Unification.mgu (litToTy(v)) (bt) (subs))

      case Exp.Var(n) => State.unit (if (!containsSc(n)(env)) throw new Exception(s"Cannot find name $n")
                                     else (findSc(n)(env)) match {
                                       case TyScheme(t, _) => Unification.mgu (Substitutions.subs(t)(subs)) (bt) (subs)
                                     })

      case Exp.Lam(b, e) => for {
        tyVarA <- newTyVar
        tyVarB <- newTyVar
        val subs1 = Unification.mgu (bt) (Type.TyLam(tyVarA, tyVarB)) (subs)
        val newEnv = addSc (b) (TyScheme(tyVarA, Set())) (env)
        ret <- tp (newEnv) (e) (tyVarB) (subs1)
      } yield ret
  }

  val predefinedEnv: Env = Env(Map[String, TyScheme]())

  def typeOf(e:Exp):Type = {
    val ret = for {
      tyVar <- newTyVar
      subs1 <- tp (predefinedEnv) (e) (tyVar) (Subst(Map[String, Type]()))
      ret <- State.unit (Substitutions.subs(tyVar)(subs1))
    } yield ret
    State.runState[Type, Int] (ret) (0)
  }
}

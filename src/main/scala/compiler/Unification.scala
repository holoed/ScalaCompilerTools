package compiler

object Unification {
  def mgu (t1:Type)(t2:Type)(sb:Subst):Subst =
    (Substitutions.subs (t1)(sb), Substitutions.subs (t2)(sb)) match {
        case (Type.TyVar(ta), Type.TyVar(tb)) if ta == tb => sb
        case (Type.TyVar(ta), _) if (!Environments.getTVarsOfType(t2).contains(ta)) => Substitutions.extend(ta)(t2)(sb)
        case (_, Type.TyVar(_)) => mgu (t2)(t1)(sb)
        case (Type.TyLam(a1, b1), Type.TyLam(a2, b2)) => mgu (a1)(a2)(mgu (b1)(b2)(sb))
        case (Type.TyCon(name1, args1), Type.TyCon(name2, args2)) if name1 == name2 =>
          (args1 zip args2).foldLeft (sb) ((acc, p) => mgu (p._1)(p._2)(acc))
        case (x, y) => throw new IllegalStateException(s"Unable to unify $x with $y")
   }
}

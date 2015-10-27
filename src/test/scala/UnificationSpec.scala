import org.scalatest._
import types._
import unifications._
import substitutions._


class UnificationSpec extends FlatSpec with Matchers {
  "The substitution resulting from the unification" should "be the given substitution if the two types are TyVar and they are the same" in {
    (Unification.mgu (Type.TyVar("a")) (Type.TyVar("a")) (Subst(Map[String, Type]()))
     should be (Subst(Map[String, Type]())))

    (Unification.mgu (Type.TyVar("a")) (Type.TyVar("a")) (Subst(Map("T" -> Type.TyVar("T"))))
     should be (Subst(Map("T" -> Type.TyVar("T")))))
  }

  it should "be the substitutions extended to include ta -> t1 if not a tvar of t2" in {
    (Unification.mgu (Type.TyVar("a")) (Type.TyVar("b")) (Subst(Map("c" -> Type.TyVar("c"))))
     should be (Subst(Map("c" -> Type.TyVar("c"), "a" -> Type.TyVar("a")))))
  }

  it should "be the substitutions extended to include tb -> t2 if not a tvar of t1" in {
    (Unification.mgu (Type.TyCon("Int", List())) (Type.TyVar("a")) (Subst(Map[String, Type]()))
     should be (Subst(Map("a" -> Type.TyVar("a")))))
  }

  it should "be the given substitution if the two function types are the same" in {
    (Unification.mgu (Type.TyLam(Type.TyVar("a"), Type.TyVar("b")))
                     (Type.TyLam(Type.TyVar("a"), Type.TyVar("b")))
                     (Subst(Map[String, Type]()))
     should be (Subst(Map[String, Type]())))
  }

  it should "be the substitutions extended to include a -> t1 if the input arg a is not a tvar of t2" in {
    (Unification.mgu (Type.TyLam(Type.TyVar("a"), Type.TyVar("b")))
                     (Type.TyLam(Type.TyVar("c"), Type.TyVar("b")))
                     (Subst(Map[String, Type]()))
     should be (Subst(Map("a" -> Type.TyVar("a")))))
  }

  it should "be the substitutions extended to include b -> t1 if the output arg b is not a tvar of t1" in {
    (Unification.mgu (Type.TyLam(Type.TyVar("a"), Type.TyVar("b")))
                     (Type.TyLam(Type.TyVar("a"), Type.TyVar("c")))
                     (Subst(Map[String, Type]()))
     should be (Subst(Map("b" -> Type.TyVar("b")))))
  }

  it should "be the substitutions extended to both input and output types if they are different" in {
    (Unification.mgu (Type.TyLam(Type.TyVar("a"), Type.TyVar("b")))
                     (Type.TyLam(Type.TyVar("c"), Type.TyVar("d")))
                     (Subst(Map[String, Type]()))
     should be (Subst(Map("b" -> Type.TyVar("b"), "a" -> Type.TyVar("a")))))
  }

  it should "be the given substitutions if the TyCon have the same name and the same tyArgs" in {
    (Unification.mgu (Type.TyCon("Int", List()))
                     (Type.TyCon("Int", List()))
                     (Subst(Map("a" -> Type.TyVar("a"))))
     should be (Subst(Map("a" -> Type.TyVar("a")))))

    (Unification.mgu (Type.TyCon("Int", List(Type.TyVar("b"))))
                     (Type.TyCon("Int", List(Type.TyVar("b"))))
                     (Subst(Map("a" -> Type.TyVar("a"))))
      should be (Subst(Map("a" -> Type.TyVar("a")))))
  }

  it should "be the substitutions extended to include a -> t1 if the tyArgs are different" in {
    (Unification.mgu (Type.TyCon("Int", List(Type.TyVar("a"))))
                     (Type.TyCon("Int", List(Type.TyVar("b"))))
                     (Subst(Map[String, Type]()))
      should be (Subst(Map("a" -> Type.TyVar("a")))))
  }
}

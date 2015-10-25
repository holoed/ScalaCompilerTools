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
}

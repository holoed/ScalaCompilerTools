import org.scalatest._
import compiler._

class SubstitutionsSpec extends FlatSpec with Matchers {
  "A substitution" should "be extended adding a (symbol, type) pair" in {
    (Substitutions.extend("T1")(Type.TyVar("T1"))(Subst(Map[String,Type]()))
     should be (Subst(Map("T1" -> Type.TyVar("T1")))))
  }

  it should "be extended when not empty" in {
    (Substitutions.extend("T")(Type.TyVar("T"))(Subst(Map("V" -> Type.TyVar("V"))))
     should be (Subst(Map("V" -> Type.TyVar("V"),
                          "T" -> Type.TyVar("T")))))
  }

  it should "be able to be looked up by symbol name" in {
    (Substitutions.lookup("T")(Subst(Map("T" -> Type.TyLam(Type.TyVar("V"), Type.TyVar("K")))))
     should be (Type.TyLam(Type.TyVar("V"), Type.TyVar("K"))))

    (Substitutions.lookup("T")(Subst(Map[String, Type]()))
     should be (Type.TyVar("T")))
  }

  "subs" should "substitute var types" in {
    (Substitutions.subs(Type.TyVar("T"))(Subst(Map("T" -> Type.TyVar("T"))))
     should be (Type.TyVar("T")))

    (Substitutions.subs(Type.TyVar("T"))(Subst(Map("T" -> Type.TyVar("V"))))
      should be (Type.TyVar("V")))

    (Substitutions.subs(Type.TyVar("T"))(Subst(Map("T" -> Type.TyLam(Type.TyVar("K"), Type.TyVar("Z")), "K" -> Type.TyVar("L"))))
      should be (Type.TyLam(Type.TyVar("L"), Type.TyVar("Z"))))
  }

  it should "substitute lambda types" in {
    (Substitutions.subs(Type.TyLam(Type.TyVar("T"), Type.TyVar("V")))(Subst(Map("T" -> Type.TyVar("K"), "V" -> Type.TyVar("Z"))))
     should be (Type.TyLam(Type.TyVar("K"), Type.TyVar("Z"))))
  }
}

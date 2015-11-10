import org.scalatest._
import compiler._

class TypeInferSpec extends FlatSpec with Matchers {
  "Type Inference" should "for a literal return the literal type" in {
    TypeInfer.typeOf (Exp.Lit(Literal.CharLit('c'))) should be (Type.TyCon("Char", List()))
  }

  it should "for a lonely var not in the environment it should throw an exception" in {
    intercept[Exception] {
      TypeInfer.typeOf (Exp.Var("x"))
    }
  }

  it should "infer same generic type for input and output in the id function" in {
    (TypeInfer.typeOf (Exp.Lam("x", Exp.Var("x")))
     should be (Type.TyLam(Type.TyVar("T2"), Type.TyVar("T2"))))
  }

  it should "infer the same type for the first argument and the output" in {
    (TypeInfer.typeOf (Exp.Lam("x", Exp.Lam("y", Exp.Var("x"))))
     should be (Type.TyLam(Type.TyVar("T4"), Type.TyLam(Type.TyVar("T3"), Type.TyVar("T4")))))
  }

  it should "infer the same type for the second argument and the output" in {
    (TypeInfer.typeOf (Exp.Lam("x", Exp.Lam("y", Exp.Var("y"))))
     should be (Type.TyLam(Type.TyVar("T1"), Type.TyLam(Type.TyVar("T4"), Type.TyVar("T4")))))
  }

  it should "infer the type of function application" in {
    (TypeInfer.typeOf (Exp.Lam("x", Exp.Lam("y", Exp.App(Exp.App(Exp.Var("+"), Exp.Var("x")), Exp.Var("y")))))
     should be (Type.TyLam(Type.TyCon("Int", List()), Type.TyLam(Type.TyCon("Int", List()), Type.TyCon("Int", List())))))

    (TypeInfer.typeOf (Exp.Lam("x", Exp.Lam("y", Exp.App(Exp.App(Exp.Var("*"), Exp.Var("x")), Exp.Var("y")))))
     should be (Type.TyLam(Type.TyCon("Int", List()), Type.TyLam(Type.TyCon("Int", List()), Type.TyCon("Int", List())))))
  }

  it should "infer the type of the tuple" in {
    (TypeInfer.typeOf (Exp.Lam("x", Exp.Lam("y", Exp.Tuple(List(Exp.Var("x"), Exp.Var("y"))))))
     should be (Type.TyLam(Type.TyVar("T5"), Type.TyLam(Type.TyVar("T6"), Type.TyCon("Tuple", List(Type.TyVar("T5"), Type.TyVar("T6")))))))

    (TypeInfer.typeOf (Exp.Lam("x", Exp.Lam("y", Exp.Tuple(List(Exp.Var("y"), Exp.Var("x"))))))
     should be (Type.TyLam(Type.TyVar("T6"), Type.TyLam(Type.TyVar("T5"), Type.TyCon("Tuple", List(Type.TyVar("T5"), Type.TyVar("T6")))))))
  }

  it should "infer the type of the let-in" in {
    (TypeInfer.typeOf (Exp.Let("x", Exp.Lit(Literal.IntegerLit(42)), Exp.Var("x")))
     should be (Type.TyCon("Int", List())))

    (TypeInfer.typeOf (Exp.Let("f", Exp.Lam("x", Exp.Var("x")), Exp.App(Exp.Var("f"), Exp.Lit(Literal.StringLit("Hello")))))
     should be (Type.TyCon("String", List())))

    (TypeInfer.typeOf (Exp.Let("f", Exp.Lam("x", Exp.App(Exp.Var("+"), Exp.Var("x"))), Exp.App(Exp.Var("f"), Exp.Lit(Literal.IntegerLit(5)))))
     should be (Type.TyLam(Type.TyCon("Int", List()), Type.TyCon("Int", List()))))
  }
}

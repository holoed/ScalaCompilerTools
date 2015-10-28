import org.scalatest._
import compiler._

class TypeInferSpec extends FlatSpec with Matchers {
  "Type Inference" should "for a literal return the literal type" in {
    TypeInfer.typeOf (Exp.Lit(Literal.CharLit('c'))) should be (Type.TyCon("char", List()))
  }

  it should "for a lonely var not found"
}

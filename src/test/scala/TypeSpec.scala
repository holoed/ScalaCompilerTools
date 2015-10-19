import org.scalatest._
import types._

class TypeSpec extends FlatSpec with Matchers {
  "A type" should "toString TyLam to its representation" in {
    Type.TyLam(Type.TyVar("a"), Type.TyVar("b")).toString should be ("a -> b")
  }

  it should "toString TyVar to its representation" in  {
    Type.TyVar("a").toString should be ("a")
  }

  it should "toString TyCon to its representation" in {
    Type.TyCon("Int", List()).toString should be ("Int")
  }
}

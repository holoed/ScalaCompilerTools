import org.scalatest._
import environments._
import types._
import scala.collection.immutable.Set

class EnvironmentsSpec extends FlatSpec with Matchers {

    "getTVarsOfType" should "return singleton set with type name for TyVar" in {
      Environments.getTVarsOfType(Type.TyVar("T1")) should be (Set("T1"))

      Environments.getTVarsOfType(Type.TyVar("T2")) should be (Set("T2"))
    }

    it should "return union of type names" in {
      (Environments.getTVarsOfType(Type.TyLam(Type.TyVar("T1"), Type.TyVar("T2")))
       should be (Set("T1", "T2")))

      (Environments.getTVarsOfType(Type.TyLam(Type.TyVar("T1"), Type.TyVar("T1")))
       should be (Set("T1")))
    }

    it should "return the folded set union of all args type names" in {
      (Environments.getTVarsOfType(Type.TyCon("Int", List(Type.TyVar("T1"), Type.TyVar("T2"))))
       should be (Set ("T1", "T2")))

      (Environments.getTVarsOfType(Type.TyCon("Int", List(Type.TyVar("T1"), Type.TyVar("T1"))))
       should be (Set("T1")))

      (Environments.getTVarsOfType(Type.TyCon("Float", List())) should be (Set()))
    }

    "getTVarsOfScheme" should "return the set difference of the TVars of type and the rest of the tvars in the scheme" in {
       (Environments.getTVarsOfScheme(TyScheme(Type.TyVar("T1"), Set("T1", "T2")))
        should be (Set()))

       (Environments.getTVarsOfScheme(TyScheme(Type.TyLam(Type.TyVar("T1"), Type.TyVar("T2")), Set("T1")))
        should be (Set("T2")))
    }
}

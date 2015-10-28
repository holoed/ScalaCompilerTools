import org.scalatest._
import compiler._

class LiteralSpec extends FlatSpec with Matchers {

  "A literal" should "toString Char to its representation" in {
    Literal.CharLit('c').toString should be ("c")
  }

  it should "toString String to its representation" in {
    Literal.StringLit("Hello World").toString should be ("Hello World")
  }

}

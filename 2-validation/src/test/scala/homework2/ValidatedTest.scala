package homework2

import org.scalatest.{FlatSpec, Matchers}

class ValidatedTest extends FlatSpec with Matchers {

  val valid = Valid(1)
  val invalid = Invalid(Chain("Error"))

  "isValid" should "check the type" in {
    valid.isValid shouldEqual true
    invalid.isValid shouldEqual false
  }

  "getOrElse" should "get the value or return default" in {
    valid.getOrElse(0) shouldEqual 1
    invalid.getOrElse(0) shouldEqual 0
  }

  "orElse" should "get the value or return default" in {
    valid.orElse(Valid(0)) shouldEqual Valid(1)
    invalid.orElse(Valid(0)) shouldEqual Valid(0)
  }

  "map" should "apply f" in {
    valid.map(_*2) shouldEqual Valid(2)
    invalid.map(_.hashCode) shouldEqual invalid
  }

  "zip" should "combine 2 Valids in a tupple" in {
    valid.zip(valid) shouldEqual Valid((1,1))
  }

  it should "return the error if one is Invalid" in {
    valid.zip(invalid) shouldEqual invalid
    invalid.zip(valid) shouldEqual invalid
  }

  it should "combine the errors of two invalids in a Chain" in {
    invalid.zip(invalid) shouldEqual Invalid(Chain("Error","Error"))
  }

  it should "work with Tuple2,3,4,5" in {
    (valid, invalid).zip shouldBe invalid
    (valid, Valid("2"), Valid(3.0)).zip shouldBe Valid((1, "2", 3.0))
    (valid, invalid, valid, Invalid(Chain("new"))).zip shouldBe Invalid(Chain("Error","new"))
    (valid,valid,valid,valid,valid).zip shouldBe Valid(1,1,1,1,1)
  }

  "map2" should "return Valid(2)" in {
    valid.map2(valid)(_ + _) shouldBe Valid(2)
    valid.map2(invalid)((_,_)) shouldBe invalid
    invalid.map2(valid)((_,_)) shouldBe invalid
  }

  "flatMap" should "apply f and flatten the result" in {
    valid.flatMap(_ => Valid(3)) shouldBe Valid(3)
    invalid.flatMap(_ => Valid(3)) shouldBe invalid
  }

  "toValidated" should "transform Option into Validated" in {
    import Validated._
    Some(1).toValidated("Error") shouldBe valid
    None.toValidated("Error") shouldBe invalid
  }

}

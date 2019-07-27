package homework2

import org.scalatest.{FlatSpec, Matchers}

class ChainTest extends FlatSpec with Matchers {

  val chain1 = Chain(1,2,3)
  val chain2 = Chain(4,5,6)

  "head" should "be 1" in {
    chain1.head shouldEqual 1
  }

  "tail" should "be Some(Chain(2,3))" in {
    chain1.tail shouldEqual Some(Chain(2,3))
  }

  it should "be None" in {
    Singleton(1).tail shouldEqual None
  }

  "isEmpty" should "be false on chain with elements" in {
    Chain(1).isEmpty shouldEqual false
  }

  it should "be true on empty chain" in {
    Chain(1).tail.isEmpty shouldEqual true
  }


  "+:" should "prepend element" in {
    3 +: chain2 shouldEqual Chain(3, 4, 5, 6)
  }

  ":+" should "append element" in {
     chain1 :+ 4 shouldEqual Chain(1, 2, 3, 4)
  }

  "++" should "append two chains" in {
    chain1 ++ chain2 shouldEqual Chain(1, 2, 3, 4, 5, 6)
  }

  "foldLeft" should "return sum of 6" in {
    chain1.foldLeft(0)(_ + _) shouldBe 6
  }

  "reduceLeft" should "return sum of 15" in {
    chain2.reduceLeft(_ + _) shouldBe 15
  }

  "map" should "multiply elements by 2" in {
    chain1.map(_ * 2) shouldEqual Chain(2,4,6)
  }

  "flatMap" should "apply f and flatten the result" in {
    chain1.flatMap(_ => Chain(1,2)) shouldEqual Chain(1,2,1,2,1,2)
  }

  "equals" should "be true" in {
    chain1.
      equals(chain1) shouldBe true
  }

  it should "be false" in {
    chain1.equals(chain2) shouldBe false
  }

  "listify" should "imitate linked list" in {
    Append(Append(Singleton(1), Singleton(2)), Append(Singleton(3), Singleton(4))).
      listify shouldEqual Append(Singleton(1), Append(Singleton(2), Append(Singleton(3), Singleton(4))))
  }

  "min" should "get min element of chain" in {
    chain1.min shouldEqual 1
  }

  "max" should "get max element of chain" in {
    chain2.max shouldEqual 6
  }
}




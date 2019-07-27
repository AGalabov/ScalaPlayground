package homework3

import java.nio.file.Paths

import org.scalatest.{FlatSpec, Matchers}
import homework3.math.Monoid
import homework3.processors.{SavedFiles, WordCount}


class MonoidTest extends FlatSpec with Matchers {

  "op" should "append two sets for setMonoid" in {
    Monoid.setMonoid[String].op(Set("url1","url2"),Set("url1","url3")) shouldBe Set("url1","url2", "url3")
  }

  it should "append two WordCount objects and update counts on matches" in {
    val operand1 = WordCount(Map("Hello" -> 1))
    val operand2 = WordCount(Map("Hello" -> 2, "world" -> 1))
    val result = WordCount(Map("Hello" -> 3, "world" -> 1))
    Monoid.wordCountMonoid.op(operand1,operand2) shouldBe result
  }

  it should "append two SavedFiles objects" in {
    val operand1 = SavedFiles(Map("url1" -> Paths.get("path1")))
    val operand2 = SavedFiles(Map("url2" -> Paths.get("path2")))
    val result = SavedFiles(Map("url1" -> Paths.get("path1"),"url2" -> Paths.get("path2")))
    Monoid.savecFilesMonoid.op(operand1,operand2) shouldBe result
  }
}

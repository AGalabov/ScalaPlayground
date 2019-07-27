package akinator

import java.nio.file.Paths

import org.scalatest.{FlatSpec, Matchers}
import akinator.tree._
import akinator.tree.Serializer._

class SerializerTest extends FlatSpec with Matchers {

  val emptyTree = Empty
  val leaf = Node("leaf",Empty,Empty)
  val testTree = Node("root",Node("child1",Node("child2"),Node("child3")),Node("child4"))

  val serializedEmpty = ""
  val serializedLeaf = "leaf(,)"
  val serializedTree = "root(child1(child2(,),child3(,)),child4(,))"

  "matchingBrackets" should "be true if opening and closing brackets count match" in {
    matchingBrackets("(())") shouldBe true
  }

  it should "be false if opening and closing brackets count don't match" in {
    matchingBrackets("((())") shouldBe false
  }

  "splitInner" should "separate a string at the first , where opening and closing brackets count match" in {
    splitInner("test1(,),test2(test3(,),)") shouldBe ("test1(,)","test2(test3(,),)")
  }

  "serialize" should "serialize an empty tree as empty String" in {
      serialize(emptyTree) shouldBe serializedEmpty
  }

  it should "serialize a leaf as value(,)" in {
    serialize(leaf) shouldBe serializedLeaf
  }

  it should "serialize a tree as root(left,righ)" in {
    serialize(testTree) shouldBe serializedTree
  }

  "deserialize" should "deserialize an empty String to Empty" in {
    deserialize(serializedEmpty) shouldBe emptyTree
  }

  it should "serialize value(,) as a Node(value,Empty,Empty)" in {
    serialize(leaf) shouldBe serializedLeaf
  }

  it should "serialize a root(left,right) as following the rule Node(root,left,right)" in {
    deserialize(serializedTree) shouldBe testTree
  }
}

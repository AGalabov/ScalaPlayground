package akinator

import akinator.tree._
import org.scalatest.{FlatSpec, Matchers}

class BinaryTreeTest extends FlatSpec with Matchers {

  val emptyTree = Empty
  val leaf = Node("leaf",Empty,Empty)
  val testTree = Node("root",Node("child1",Node("child2"),Node("child3")),Node("child4"))

  "rootValue" should "be None on empty tree" in {
      emptyTree.rootValue shouldBe None
  }

  it should "be Some(value) for non empty tree" in {
    testTree.rootValue shouldBe Some("root")
  }

  "leftSubTree" should "be None on empty tree" in {
    emptyTree.leftSubTree shouldBe None
  }

  it should "be None for tree leaf" in {
    leaf.leftSubTree shouldBe None
  }

  it should "be Some(value) for non empty tree" in {
    testTree.leftSubTree shouldBe Some(Node("child1",Node("child2"),Node("child3")))
  }

  "rightSubTree" should "be None on empty tree" in {
    emptyTree.rightSubTree shouldBe None
  }

  it should "be None for tree leaf" in {
    leaf.rightSubTree shouldBe None
  }

  it should "be Some(rightSubTree) for non empty tree" in {
    testTree.rightSubTree shouldBe Some(Node("child4"))
  }
}

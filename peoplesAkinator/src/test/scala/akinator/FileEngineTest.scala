package akinator

import akinator.tree.FileEngine._
import akinator.tree.Node
import org.scalatest.{FlatSpec, Matchers}

class FileEngineTest extends FlatSpec with Matchers {

  val testTree = Node("root",Node("child1"),Node("child2"))

  "writeTreeToFile" should "write a serialized tree to file" in {
    val filePath = "./testFiles/testOutput.txt"
    writeTreeToFile(testTree,filePath)
    val fileOutput = readStringFromFile(filePath)
    fileOutput shouldBe "root(child1(,),child2(,))"
  }
}

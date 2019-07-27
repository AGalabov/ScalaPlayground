package akinator.tree

import java.io.{File, PrintWriter}

import scala.io.Source

object FileEngine {
  def writeTreeToFile(tree: Tree[String], filePath : String = "tree.txt"): Unit = {
    val pw = new PrintWriter(new File(filePath ))
    pw.write(Serializer.serialize(tree))
    pw.close()
  }

  def readStringFromFile(filePath : String = "tree.txt"): String = {
    val source = Source.fromFile(filePath)
    val str = source.mkString
    source.close()
    str
  }
}
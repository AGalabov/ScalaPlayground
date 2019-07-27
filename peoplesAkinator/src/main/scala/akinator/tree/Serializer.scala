package akinator.tree

object Serializer {
  private val pattern = "^([^\\(]*)\\((.*)\\)$".r
  private val BRACKET_OPEN = "("
  private val BRACKET_CLOSE = ")"
  private val SEPARATOR = ","
  private val SEPARATOR_LENGTH = 1

  def serialize(tree: Tree[String]): String = tree match {
    case Node(value, left, right) =>
      val leftStr = serialize(left)
      val rightStr = serialize(right)
      s"$value$BRACKET_OPEN$leftStr$SEPARATOR$rightStr$BRACKET_CLOSE"
    case Empty => ""
  }

  def deserialize(str: String): Tree[String] = str match {
    case pattern(value, inner) =>
      val (left, right) = splitInner(inner)
      Node(value, deserialize(left), deserialize(right))
    case _ => Empty
  }

  def matchingBrackets(str : String): Boolean = str.count(_ == '(') == str.count(_ == ')')

  def splitInner(inner: String): (String, String) = {
    val left = 0.until(inner.length)
      .filter(inner.startsWith(SEPARATOR,_))
      .map(i => inner.slice(0,i))
      .find(matchingBrackets)
      .getOrElse("")
    val right = inner.slice(left.length + SEPARATOR_LENGTH,inner.length)
    (left,right)
  }
}
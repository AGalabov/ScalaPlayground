package akinator.tree

sealed trait Tree[+A] {

  def rootValue: Option[A] = this match {
    case Node(x,_,_) => Some(x)
    case Empty      => None
  }

  def leftSubTree: Option[Tree[A]] = this match {
    case Node(_,Empty,Empty) => None
    case Node(_,l,_) => Some(l)
    case _      => None
  }

  def rightSubTree: Option[Tree[A]] = this match {
    case Node(_,Empty,Empty) => None
    case Node(_,_,r) => Some(r)
    case Empty      => None
  }
}

case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
case object Empty extends Tree[Nothing]

object Node {
  def apply(value: String) : Tree[String] = Node(value, Empty, Empty)
}
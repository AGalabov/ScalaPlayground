package homework3.math
import java.nio.file.Path

import homework3.processors._

trait Monoid[M] {
  def op(a: M, b: M): M
  def identity: M
}

object Monoid {
  def apply[A](implicit m: Monoid[A]): Monoid[A] = m

  object ops {
    implicit class MonoidOps[A](val a: A) extends AnyVal {
      def |+|(b: A)(implicit m: Monoid[A]): A = m.op(a, b)
    }
  }

  implicit val intAdditiveMonoid: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a + b
    val identity: Int = 0
  }

  val intMultiplicativeMonoid: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a * b
    val identity: Int = 1
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a: String, b: String): String = a + b
    val identity: String = ""
  }

  implicit def optionMonoid[A : Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    import ops._

    def op(a:  Option[A], b:  Option[A]): Option[A] = (a, b) match {
      case (Some(n), Some(m)) => Some(n |+| m)
      case (Some(_), _) => a
      case (_, Some(_)) => b
      case _ => None
    }

    def identity: Option[A] = None
  }

  implicit def pairMonoid[A : Monoid, B : Monoid]: Monoid[(A, B)] = new Monoid[(A, B)] {
    import ops._
    def op(a: (A, B), b: (A, B)): (A, B) = (a, b) match {
      case ((a1, a2), (b1, b2)) => (a1 |+| b1, a2 |+| b2)
    }

    def identity: (A, B) = (Monoid[A].identity, Monoid[B].identity)
  }

  implicit def mapMonoid[K, V : Monoid]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    import ops._

    def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
      val vIdentity = Monoid[V].identity

      (a.keySet ++ b.keySet).foldLeft(identity) { (acc, key) =>
        acc + (key -> (a.getOrElse(key, vIdentity) |+| b.getOrElse(key, vIdentity)))
      }
    }
    def identity: Map[K, V] = Map.empty[K, V]
  }

  implicit def setMonoid[O : Monoid]: Monoid[Set[O]] = new Monoid[Set[O]] {
    def op(a: Set[O], b: Set[O]): Set[O] = a ++ b
    def identity: Set[O] = Set.empty[O]
  }

  implicit val wordCountMonoid: Monoid[WordCount] = new Monoid[WordCount] {
    def op(a: WordCount, b: WordCount): WordCount =
      WordCount(mapMonoid[String,Int].op(a.wordToCount,b.wordToCount))

    def identity: WordCount = WordCount(Map.empty[String, Int])
  }

  implicit val savecFilesMonoid: Monoid[SavedFiles] = new Monoid[SavedFiles] {
    def op(a: SavedFiles, b: SavedFiles): SavedFiles = {
      SavedFiles(a.urlToPath ++  b.urlToPath)
    }

    def identity: SavedFiles = SavedFiles(Map.empty[String, Path])
  }
}
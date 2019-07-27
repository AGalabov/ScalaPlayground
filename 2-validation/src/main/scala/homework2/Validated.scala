package homework2

sealed trait Validated[+E, +A] {
  def isValid: Boolean = this match {
    case Valid(_) => true
    case _ => false
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Valid(a) => a
    case Invalid(_) => default
  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] = this match {
    case Valid(_) => this
    case Invalid(_) => default
  }

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = map2(vb)((_,_))

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a) => Valid(f(a))
    case Invalid(err) => Invalid(err)
  }

  def map2[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = (this,vb) match {
    case (Valid(a),Valid(b)) => Valid(f(a,b))
    case (Valid(_),Invalid(err)) => Invalid(err)
    case (Invalid(err),Valid(_)) => Invalid(err)
    case (Invalid(err1),Invalid(err2)) => Invalid(Append(err1,err2))
  }

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = this match {
    case Valid(a) => f(a)
    case Invalid(err) => Invalid(err)
  }

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {
    case Valid(a) => valid(a)
    case Invalid(err) => invalid(err)
  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)
}

case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))
}

object Validated {
  //def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] = ???

  implicit class ValidatedTuple2[EE, A, B](val tuple: (Validated[EE, A], Validated[EE, B])) extends AnyVal {
    implicit def zip: Validated[EE, (A, B)] = tuple._1.zip(tuple._2)
    def zipMap[R](f: (A, B) => R): Validated[EE, R] = tuple._1.map2(tuple._2)(f)
  }

  implicit class ValidatedTuple3[EE, A, B, C](val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C])) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] = zipMap((_,_,_))
    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = tuple match {
      case (Valid(a), Valid(b), Valid(c)) => Valid(f(a, b, c))
      case _ => tuple.productIterator.
        map(x => x.asInstanceOf[Validated[EE, Any]]).
        filterNot(_.isValid).
        reduceLeft((a, b) => a.zip(b).asInstanceOf[Invalid[EE]]).asInstanceOf[Validated[EE, R]]
    }
  }

  implicit class ValidatedTuple4[EE, A, B, C, D]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] = zipMap((_,_,_,_))
    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = tuple match {
      case (Valid(a), Valid(b), Valid(c), Valid(d)) => Valid(f(a, b, c, d))
      case _ => tuple.productIterator.
        map(x => x.asInstanceOf[Validated[EE, Any]]).
        filterNot(_.isValid).
        reduceLeft((a, b) => a.zip(b).asInstanceOf[Invalid[EE]]).asInstanceOf[Validated[EE, R]]
    }
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D], Validated[EE, E])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] = zipMap((_, _, _, _, _))

    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] = tuple match {
      case (Valid(a), Valid(b), Valid(c), Valid(d), Valid(e)) => Valid(f(a, b, c, d, e))
      case _ => tuple.productIterator.
        map(x => x.asInstanceOf[Validated[EE, Any]]).
        filterNot(_.isValid).
        reduceLeft((a, b) => a.zip(b).asInstanceOf[Invalid[EE]]).asInstanceOf[Validated[EE, R]]
    }
  }

  implicit class ValidatedOption[A](val option: Option[A]) extends AnyVal {
    def toValidated[E](error: E): Validated[E, A] = option match {
      case None => Invalid(error)
      case Some(value) => Valid(value)
    }
  }
}

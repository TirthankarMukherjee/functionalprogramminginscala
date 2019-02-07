package datasharing

/*

# It’s fine to use pattern matching, though you should be able to implement all
the functions besides map and getOrElse without resorting to pattern matching.

# For map and flatMap, the type signature should be enough to determine the
implementation.

# getOrElse returns the result inside the Some case of the Option, or if the Option
is None, returns the given default value.

# orElse returns the first Option if it’s defined; otherwise, it returns the second
Option.

 */


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option[B]): Option[B]

  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {

  override def map[B](f: A => B): Option[B] = this match {
    case Some(a: A) => Some(f(a))
    case _ => None
  }

  override def flatMap[B](f: A => Option[B]): Option[B] = ???

  override def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  override def orElse[B >: A](ob: => Option[B]): Option[B] = ob(get)

  override def filter(f: A => Boolean): Option[A] = if(f(get)) Some(get) else None
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(f: Nothing => Boolean): Option[Nothing] = None
}



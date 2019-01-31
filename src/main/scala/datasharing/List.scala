package datasharing

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, xs: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Cons(x, xs) => x + sum(xs)
    case Nil => 0
  }

  def product(ints: List[Int]): Int = ints match {
    case Cons(x, xs) => x * product(xs)
    case Cons(0, _) => 0
    case Nil => 1

  }

  //FPS - Ex 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, xs) => xs
    case _ => Nil
  }

  //FPS - Ex 3.3
  def setHead[A](l: List[A], head: A): List[A] = l match {
    case Cons(_, xs) => Cons(head, xs)
    case _ => sys.error("setHead on empty list")
  }

  //FPS - Ex 3.4
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, _) if (n == 0) => l
    case Cons(_, xs) => drop(xs, n - 1)
    case _ => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case Cons(x, xs) if !f(x) => l
    case _ => Nil
  }

  //Improving type inference for higher-order functions
  def dropWhileWithCurry[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhileWithCurry(xs)(f)
    case _ => l
  }

  //  foldRight is not specific to any one type of element, and we discover while generalizing
  //  that the value that’s returned doesn’t have to be of the same type as the elements
  //  of the list
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {

    case Cons(h, t) => f(h, foldRight(t, z)(f))
    case _ => z
  }

  //3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  //3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, y) => 1 + y)
  }

  // 3.11
  def size[A](l: List[A]): Int = {
    foldLeft(l, 0)((x, _) => x + 1)
  }

  //3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((acc, h) => Cons(h, acc))
  }

  //3.14
  def append[A](as: List[A], element: A): List[A] = {
    foldRight(as, List(element))((h, t) => Cons(h, t))
    // foldRight(as, List(element))( Cons(_,_))
  }

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  // My concatenate looks like this, as I miss understood earlier problem of append, where I was adding a single
  // element, but in end all looks sa,e ;)
  //3.15
  def concatenate[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])((h, t) =>
      foldLeft(t, h)((acc, hh) =>
        append(acc, hh)))
  }

  //3.15
  def concatenateBetter[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])(append)
  }


  //3.16 , 3.17, 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((h, acc) => Cons(f(h), acc))
  }

  //3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]())((h, acc) => if (f(h)) Cons(h, acc) else acc)
  }

  //3.20
  def flatmap[A](as: List[A])(f: A => List[A]): List[A] = {
    foldRight(as, Nil: List[A])((h, acc) => append(f(h), acc))
  }

  //3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatmap(as)(x => if (f(x)) List(x) else Nil)
  }



  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def main(args: Array[String]): Unit = {
    println(sum(List(1, 2, 3, 4)))
    println(product(List(1, 2, 3, 4)))
    println(tail(List(1, 2, 3, 4)))

    println(drop(List(1, 2, 3, 4), 2))
    println(dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3))
    println(dropWhileWithCurry(List(1, 2, 3, 4))(x => x < 3))

    println(foldRight(List(1, 2, 3, 4, 5), 0)((x, y) => x + y))
    println(foldRight(List(1, 2, 3, 4, 5), 0)(_ + _))
    println(foldRight(List(1, 2, 3, 4, 5), 1)((x, y) => x * y))
    println(foldRight(List("a", "b", "c"), "")((x, y) => x + y))
    println(length(List("a", "b", "c")))

    println(foldRight(List(1, 2, 3, 4, 5), Nil: List[Int])(Cons(_, _)))

    println(foldLeft(List(1, 2, 3, 4, 5), 0)((x, y) => x + y))
    println(foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _))
    println(foldLeft(List(1, 2, 3, 4, 5), 1)((x, y) => x * y))
    println(foldLeft(List("a", "b", "c"), "")((x, y) => x + y))
    println(size(List("a", "b", "c")))

    println(reverse(List("a", "b", "c")))

    println(append(List("a", "b", "c"), "d"))
    println(concatenate(List(List(1, 2, 3, 4), List(5, 6, 7, 8))))
    println(concatenateBetter(List(List(1, 2, 3, 4), List(5, 6, 7, 8))))

    println(map(List(1, 2, 3, 4))(_ + 1))
    //3.17
    println(map[Double, String](List[Double](1.0, 2.0, 3.0, 4.0))(_.toString))

    println(filter(List(1, 5, 2, 3, 4, 3))(x => x < 4))
    //3.20
    println(flatmap(List(1, 2, 3, 4, 5))(x => List(x, x)))

    //3.21
    println(filterWithFlatMap(List(1, 5, 2, 3, 4, 3))(x => x < 4))
  }
}








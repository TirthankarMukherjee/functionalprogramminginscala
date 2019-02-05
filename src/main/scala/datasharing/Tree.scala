package datasharing

sealed trait Tree[+A] {
	override def toString: String = super.toString
}

case class Leaf[+A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]




object Tree {

	def size[A](tree: Tree[A]): Int = tree match {
		case Leaf(_: A) => 1
		case Branch(left, right) => 1 + size(left) + size(right)
	}

	def map[A,B](tree : Tree[A])(f : A => B) : Tree[B] = tree match {
		case Leaf(x: A) => Leaf(f(x))
		case Branch(left, right) => Branch(map(left)(f), map(right)(f))

	}

	def main(args: Array[String]): Unit = {
		val branchLeft: Branch[String] = new Branch(Leaf("a"), Leaf("b"))
		val branchRight: Branch[String] = new Branch(Leaf("c"), Leaf("d"))
		val tree: Tree[String] = new Branch[String](branchLeft, branchRight)

		print(size(tree)) // 7
	}
}

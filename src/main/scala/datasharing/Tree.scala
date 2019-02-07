package datasharing

sealed trait Tree[+A] {
	override def toString: String = super.toString
}

case class Leaf[+A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	//3.25
	def size[A](tree: Tree[A]): Int = tree match {
		case Leaf(_: A) => 1
		case Branch(left, right) => 1 + size(left) + size(right)
	}

	//3.26
	def max(tree : Tree[Int]) : Int = {
		findMax(tree,-9999)
	}
	private def findMax(tree: Tree[Int], min : Int) : Int = tree match {
		case Leaf( x: Int) => x.max(min)
		case Branch(left, right) => findMax(left, min).max(findMax(right, min))
	}

	//3.28
	def map[A,B](tree : Tree[A])(f : A => B) : Tree[B] = tree match {
		case Leaf(x: A) => Leaf(f(x))
		case Branch(left, right) => Branch(map(left)(f), map(right)(f))

	}

	//3.29
	//Implement fold
	// ¯\_(ツ)_/¯

	def main(args: Array[String]): Unit = {
		val branchLeft: Branch[String] = new Branch(Leaf("a"), Leaf("b"))
		val branchRight: Branch[String] = new Branch(Leaf("c"), Leaf("d"))
		val tree: Tree[String] = new Branch[String](branchLeft, branchRight)

		val branchLeftN: Branch[Int] = new Branch(Leaf(23), Leaf(0))
		val branchRightN: Branch[Int] = new Branch(Leaf(45), Leaf(33))
		val treeN: Tree[Int] = new Branch[Int](branchLeftN, branchRightN)

		println(size(tree)) // 7

		map(map(tree)(_.toUpperCase + 22))(println)

		println(max(treeN))

	}
}

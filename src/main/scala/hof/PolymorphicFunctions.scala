package hof

//Polymorphic function to find an element in an array - 2.4

object PolymorphicFunctions {

  private def findFirst[A](arr: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= arr.length) -1
      else if (p(arr(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  //Implement isSorted , which checks whether an Array[A] is sorted according to a
  //given comparison function: Ex - 2.2 , pg -24
  private def isSorted[A](arr: Array[A], p: (A, A) => Boolean): Boolean = {
    def loop(n:Int) : Boolean = {
      if(n+1 >= arr.length) true
      else if(p(arr(n),arr(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(findFirst(Array(23, 12, 45, 50), (a: Int) => a > 40))
    println(findFirst(Array("Hello", "apple", "sun", "yellow banana"), (a: String) => a.length > 6))
    println(isSorted(Array(1,2,3,4,5,6), (a:Int,b:Int) => a < b))
    println(isSorted(Array(1,2,3,5,4,6), (a:Int,b:Int) => a < b))
  }
}

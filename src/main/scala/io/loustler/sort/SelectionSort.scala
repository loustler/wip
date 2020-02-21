package io.loustler.sort

object SelectionSort {

  def sort[A](list: List[A])(implicit Ord: Ordering[A]): List[A] = {
    import Ord._

    def iSort(x: List[A], b: List[A]): List[A] = x match {
      case head :: tail =>
        println(s"sort => original: $x, sorted: $b, head: $head, tail: $tail")
        select(head, tail, Nil, b)

      case Nil =>
        println(s"sort => original: $x, sorted: $b")
        b
    }

    @scala.annotation.tailrec
    def select(a: A, x: List[A], y: List[A], z: List[A]): List[A] = x match {
      case head :: tail =>
        println(s"select => pivot: $a, head: $head, tail: $tail, subList: $y, sorted: $z")
        if (a > head) select(a, tail, head :: y, z)
        else select(head, tail, a :: y, z)

      case Nil => {
        println(s"select => pivot: $a, subList: $y, sorted: $z")
        iSort(y, a :: z)
      }
    }

    iSort(list, Nil)
  }
}

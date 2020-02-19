package io.loustler.sort

object InsertionSort {

  def sort[A](list: List[A])(implicit Ord: Ordering[A]): List[A] = {
    import Ord._

    @scala.annotation.tailrec
    def sort_(x: List[A], y: List[A]): List[A] = x match {
      case head :: tail => {
        println(s"sort x: $x, y: $y, head: $head, tail: $tail")
        sort_(tail, insert(head, y))
      }

      case Nil =>
        println(s"sort x:$x, y: $y")
        y
    }

    def insert(x: A, xs: List[A]): List[A] = xs match {
      case head :: tail if x > head => {
        println(s"insert x: $x, xs: $xs, head: $head, tail: $tail")
        head :: insert(x, tail)
      }

      case _ =>
        println(s"insert x: $x, xs: $xs")
        x :: xs
    }

    sort_(list, Nil)
  }
}

package io.loustler.sort

/**
  * @author loustler
  * @since 02/18/2020
  */
object BubbleSort {

  /**
    * @example {{{
    *   scala> val list = List(5, 1, 6, 2, 4, 3)
    *   scala> BubbleSort.sort(list)
    * }}}
    *
    * Progress Like:
    * {{{
    *   A. 5 1 6 2 4 3
    *   B. 1 5 6 2 4 3
    *   C. 1 5 2 6 4 3
    *   D. 1 5 2 4 6 3
    *   E. 1 5 2 4 3 6
    *   F. 1 5 2 4 3 6
    *   G. 1 2 5 4 3 6
    *   H. 1 2 4 5 3 6
    *   I. 1 2 4 3 5 6
    *   J. 1 2 4 3 5 6
    *   K. 1 2 4 3 5 6
    *   L. 1 2 3 4 5 6
    * }}}
    * @param list list
    * @param Ord  ordering for compare list
    * @tparam A type A
    * @return
    */
  def sort[A](list: List[A])(implicit Ord: Ordering[A]): List[A] = {
    import Ord._

    def sort(x: List[A], y: List[A]): List[A] =
      if (x.isEmpty) y
      else bubble(x, Nil, y)

    @scala.annotation.tailrec
    def bubble(x: List[A], y: List[A], z: List[A]): List[A] = x match {
      case head1 :: head2 :: tail =>
        println(s"head1: $head1, head2: $head2, x: ${x}, y: ${y}, z: ${z}")
        if (head1 > head2) bubble(head1 :: tail, head2 :: y, z)
        else bubble(head2 :: tail, head1 :: y, z)

      case head :: Nil =>
        println(s"head: $head, y: $y, z: $z")
        sort(y, head :: z)
    }

    sort(list, Nil)
  }

  /**
    * @example {{{
    *    scala> val list = List(1, 4, 2, 3, 5, 8, 6, 9, 7)
    *    scala> BubbleSort.sort(list)(_ > _) // List(9, ...., 1)
    *    scala> BubbleSort.sort(list)(_ < _) // List(1, ...., 9)
    * }}}
    *
    * @param list list
    * @param f HOF(Higher Order Function) for compare list each element
    * @tparam A type A
    * @return
    */
  def sort2[A](list: List[A])(f: (A, A) => Boolean): List[A] = {
    def sort(x: List[A], y: List[A]): List[A] =
      if (x.isEmpty) y
      else bubble(x, Nil, y)

    @scala.annotation.tailrec
    def bubble(x: List[A], y: List[A], z: List[A]): List[A] = x match {
      case head1 :: head2 :: tail =>
        println(s"head1: $head1, head2: $head2, x: ${x}, y: ${y}, z: ${z}")
        if (f(head1, head2)) bubble(head1 :: tail, head2 :: y, z)
        else bubble(head2 :: tail, head1 :: y, z)

      case head :: Nil =>
        println(s"head: $head, y: $y, z: $z")
        sort(y, head :: z)
    }

    sort(list, Nil)
  }
}

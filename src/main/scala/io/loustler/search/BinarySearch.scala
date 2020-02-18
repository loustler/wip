package io.loustler.search

/**
  * @author loustler
  * @since 02/18/2020
  */
object BinarySearch {

  /**
    * {{{
    *   scala> val list = List(5, 3, 2, 1, 6, 7).sorted // List(1, 2, 3, 5, 6, 7)
    *   scala> val resultA = BinarySearch.search(list, 4) // None
    *   scala> val resultB = BinarySearch.search(list, 3) // Some(3)
    * }}}
    *
    * @param list list
    * @param key  key to find
    * @param Ord  Ordering for type A
    * @tparam A type A
    * @return Some[A], otherwise None
    */
  def search[A](list: List[A], key: A)(implicit Ord: Ordering[A]): Option[A] = {
    import Ord._

    def search(left: List[A], right: List[A]): Option[A] =
      if (left == right) None
      else test(left, right, middle(left, right))

    def test(left: List[A], right: List[A], middle: List[A]): Option[A] =
      if (key < middle.head) search(left, middle)
      else if (key > middle.head)
        search(
          middle.tail,
          right
        )
      else
        Some(
          middle.head
        )

    /**
      *
      *
      * @param left  left side list
      * @param right right side list
      * @return
      */
    def middle(left: List[A], right: List[A]): List[A] = {
      @scala.annotation.tailrec
      def race(tail: List[A], head: List[A]): List[A] =
        if (head != right && head.tail != right)
          race(
            tail.tail,
            head.tail.tail
          )
        else tail

      race(left, left.tail)
    }

    search(list, Nil)
  }
}

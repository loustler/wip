package io.loustler.sort

object MergeSort {

  def sort[A](list: List[A])(implicit Ord: Ordering[A]): List[A] = {
    import Ord._

    def sort_(pivot: (List[A], List[A])): List[A] = pivot match {
      case (Nil, Nil)          => Nil
      case (headA :: Nil, Nil) => headA :: Nil
      case (Nil, headB :: Nil) => headB :: Nil
      case (x, y)              => merge(halfifyAndSort(x), halfifyAndSort(y))
    }

    def halfify(x: List[A]): (List[A], List[A]) = {
      @scala.annotation.tailrec
      def loop(y: List[A], fs: List[A], ss: List[A]): (List[A], List[A]) = y match {
        case f :: s :: r => loop(r, f :: fs, s :: ss)
        case f :: Nil    => (f :: fs, ss)
        case Nil         => (fs, ss)
      }

      loop(x, Nil, Nil)
    }

    def merge(x: List[A], y: List[A]): List[A] = {
      def loop(a: List[A], b: List[A], r: List[A]): List[A] = (a, b) match {
        case (headA :: tailA, headB :: tailB) =>
          if (headA < headB) loop(tailA, b, headA :: r)
          else loop(a, tailB, headB :: r)

        case (headA :: tailA, Nil) => loop(tailA, Nil, headA :: r)
        case (Nil, headB :: tailB) => loop(Nil, tailB, headB :: r)
        case (Nil, Nil)            => r
      }

      loop(x, y, Nil).reverse
    }

    def halfifyAndSort(x: List[A]): List[A] = sort_(halfify((x)))

    halfifyAndSort(list)
  }
}

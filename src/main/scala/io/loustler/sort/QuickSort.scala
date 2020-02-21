package io.loustler.sort

/**
  * QuickSort
  *
  * Best: O(n log n)
  * Avg: O(n log n)
  * Worst: O(n^2)
  *
  * How to work:
  * List(3, 2, 4, 9, 6, 1, 8, 7)
  * {{{
  *   1. pick pivot(head: 3)
  *   2. Create 2 empty list
  *   3. compare pivot with every single head which from tail
  * }}}
  *
  * Detail:
  * {{{
  *   1. 3, 2, 4, 9, 6, 1, 8, 7 (pick pivot, 3 is pivot in here)
  *   2. 3 compare 2, 2 adds to less list
  *   3. 3 compare 4, 4 adds to greater list
  *   4. 3 compare 9, 9 adds to greater list
  *   5. 3 compare 6, 6 adds to greater list
  *   6. 3 compare 1, 1 adds to less list
  *   7. 3 compare 8, 8 adds to greater list
  *   8. 3 compare 7, 7 adds to greater list
  *   9. less: List(1, 2), greater: List(7, 8, 6, 9, 4)
  *   10. 1 compare 2, 2 adds to greater list <- less list sort start
  *   11. less: List(), greater: List(2) <- less list sort done
  *   12. 7 compare 8, 8 adds to greater list <- greater list sort start
  *   13. 7 compare 6, 6 adds to less list
  *   14. 7 compare 9, 9 adds to greater list
  *   15. 7 compare 4, 4 adds to less list
  *   16. greaterLess: List(4, 6), greaterGreater: List(9, 8)
  *   17 ... again..
  * }}}
  */
object QuickSort {

  def sort[A](list: List[A])(implicit Ord: Ordering[A]): List[A] = {
    import Ord._

    def iSort(t: (List[A], A, List[A])): List[A] = t match {
      case (Nil, pivot, Nil) => List(pivot)

      case (less, pivot, greater) => partitionAndSort(less) ::: (pivot :: partitionAndSort(greater))
    }

    def partition(x: List[A]): (List[A], A, List[A]) = {
      @scala.annotation.tailrec
      def loop(pivot: A, y: List[A], less: List[A], greater: List[A]): (List[A], A, List[A]) = y match {
        case head :: tail => {
          println(s"partition => pivot: $pivot, head: $head, tail: $tail, less: $less, greater: $greater")
          if (head < pivot) loop(pivot, tail, head :: less, greater) // less쪽에 cons
          else loop(pivot, tail, less, head :: greater) // greater에 cons
        }

        case Nil => {
          println(s"partition => pivot: $pivot, less: $less, greater: $greater")
          (less, pivot, greater)
        }
      }

      loop(x.head, x.tail, Nil, Nil)
    }

    def partitionAndSort(x: List[A]): List[A] = {
      println(s"Start partition and sort with $x")
      if (x.isEmpty) Nil
      else iSort(partition(x))
    }

    partitionAndSort(list)
  }
}

package io.loustler.sort

import org.scalatest.flatspec.AnyFlatSpecLike

final class QuickSortTest extends AnyFlatSpecLike {
  import QuickSort.sort

  "QuickSort" should "be succeed" in {
    val list = List(6, 1, 4, 3, 9, 5, 8, 7)

    val result = sort(list)

    assertResult(1)(result.head)
    assertResult(9)(result.last)
    assertResult(List(1, 3, 4, 5, 6, 7, 8, 9))(result)
  }
}

package io.loustler.sort

import org.scalatest.flatspec.AnyFlatSpecLike

final class SelectionSortTest extends AnyFlatSpecLike {
  import SelectionSort.sort

  "SelectionSort" should "be succeed" in {
    val list = List(4, 1, 7, 6, 9, 2, 8, 3, 5)

    val result = sort(list)

    assertResult(1)(result.head)
    assertResult(9)(result.last)
    assertResult(List(1, 2, 3, 4, 5, 6, 7, 8, 9))(result)
  }
}

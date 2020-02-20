package io.loustler.sort

import org.scalatest.flatspec.AnyFlatSpecLike

final class MergeSortTest extends AnyFlatSpecLike {
  import MergeSort.sort

  "MergeSort" should "be succeed" in {
    val list = List(6, 3, 8, 2, 3, 4, 9, 1)

    val result = sort(list)

    assertResult(1)(result.head)
    assertResult(9)(result.last)
  }
}

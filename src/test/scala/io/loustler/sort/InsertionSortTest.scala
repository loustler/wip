package io.loustler.sort

import org.scalatest.flatspec.AnyFlatSpecLike

final class InsertionSortTest extends AnyFlatSpecLike {
  import InsertionSort.sort

  "InsertionSort" should "be succeed" in {
    val list = List(5, 4, 2, 8, 7, 9)

    val result = sort(list)

    assertResult(result.head)(2)
    assertResult(result.last)(9)
  }
}

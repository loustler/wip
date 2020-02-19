package io.loustler.sort

import org.scalatest.flatspec.AnyFlatSpecLike

class BubbleSortTest extends AnyFlatSpecLike {
  import BubbleSort.sort

  "BubbleSort" should "be succeed" in {
    val list = List(1, 4, 5, 6, 2, 9, 3, 7)

    val sorted = sort(list)

    println(sorted)

    assertResult(1)(sorted.head)
    assertResult(9)(sorted.last)
  }
}

package io.loustler.search

import org.scalatest.flatspec.AnyFlatSpecLike

final class BinarySearchTest extends AnyFlatSpecLike {
  import BinarySearch.search

  "BinarySearch" should "be succeed if not sorted list" in {
    val list = List(1, 4, 2, 3, 6, 7)

    val resultA = search(list, 9)

    assert(resultA.isEmpty === true)

    val resultB = search(list, 7)

    assert(resultB.isDefined)
  }

  it should "be succeed" in {
    val list = List(1, 4, 2, 3, 6, 7).sorted

    val resultA = search(list, 9)

    assert(resultA.isEmpty === true)

    val resultB = search(list, 7)

    assert(resultB.isDefined)
  }
}

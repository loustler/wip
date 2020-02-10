package io.loustler.collection

import org.scalatest.flatspec.AnyFlatSpecLike

final class ListTest extends AnyFlatSpecLike {
  "List" should "check empty" in {
    val list = List.empty

    assert(list.isEmpty === true)
    assertResult(0)(list.length)
  }

  it should "be successful when not empty" in {
    val list: List[Int] = List(1)

    assertResult(List(0, 1))(list.prepend(0))
    assertResult(List(1, 2))(list.append(2))
    assertResult(List(1, 2, 3, 4))(list ::: List(2, 3, 4))
    assertResult(List(1, 2, 3))(list.concat(List(2, 3)))
    assert(list.remove(1) === Nil)
    assert(list.contains(1) === true)
    assert(list.contains(2) === false)
  }
}

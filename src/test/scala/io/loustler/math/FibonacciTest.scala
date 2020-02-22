package io.loustler.math

import org.scalatest.flatspec.AnyFlatSpecLike

final class FibonacciTest extends AnyFlatSpecLike {
  import Fibonacci._

  // 0 1 1 2 3 5 8...
  "Fibonacci" should "be succeed" in {
    assertResult(2)(fibonacci(3))

    assertResult(5)(fibonacci(5))

    assertResult(55)(fibonacci(10))
  }
}

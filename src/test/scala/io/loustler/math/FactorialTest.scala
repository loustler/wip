package io.loustler.math

import org.scalatest.flatspec.AnyFlatSpecLike

final class FactorialTest extends AnyFlatSpecLike {
  import Factorial.factorial

  "Factorial" should "be succeed" in {
    assertResult(1)(factorial(1))
    assertResult(120)(factorial(5))
    assertResult(479001600)(factorial(12))
  }
}

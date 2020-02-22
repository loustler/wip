package io.loustler.math

object Factorial {

  /**
    * Factorial
    * {{{
    *   1! = 1
    *   3! = 1 * 2 * 3
    *   5! = 1 * 2 * 3 * 4 * 5
    * }}}
    *
    * @param n factorial number
    * @return
    */
  def factorial(n: Int): Long = {
    def loop(n: Int): Long = n match {
      case n if n <= 1 => 1
      case n           => loop(n - 1) * n
    }

    loop(n)
  }
}

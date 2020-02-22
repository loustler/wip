package io.loustler.math

object Fibonacci {

  /**
    * {{{
    *   0 1 1 2 3 5 8 13 ....
    * }}}
    *
    * @param n fibonacci number
    * @return
    */
  def fibonacci(n: Int): Int = {
    def loop(n: Int): Int = n match {
      case n if n <= 0 => 0
      case n if n == 1 => n
      case n           => loop(n - 2) + loop(n - 1)
    }

    loop(n)
  }
}

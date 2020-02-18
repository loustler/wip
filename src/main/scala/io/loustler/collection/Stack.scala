package io.loustler.collection

final class Stack[+A](self: List[A]) {
  def top: A = self.head

  def rest: Stack[A] = new Stack(self.tail)

  def isEmpty: Boolean = self.isEmpty

  def pop: (A, Stack[A]) = (top, rest)

  def push[B >: A](b: B): Stack[B] = new Stack(b :: self)
}

object Stack {
  def empty[A]: Stack[A] = new Stack(Nil)

  def apply[A](xs: A*): Stack[A] =
    xs.foldLeft(empty[A]) {
      case (r, x) => r.push(x)
    }
}

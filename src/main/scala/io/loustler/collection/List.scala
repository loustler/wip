package io.loustler.collection

import scala.annotation.tailrec

/**
  * LinkedList
  */
sealed abstract class List[+A] {
  def head: A

  def tail: List[A]

  def isEmpty: Boolean

  def append[B >: A](x: B): List[B] =
    if (isEmpty) List.make(x)
    else List.make(head, tail.append(x))

  def prepend[B >: A](x: B): List[B] = List.make(x, this)

  def ::[B >: A](x: B): List[B] = prepend(x)

  def :::[B >: A](prefix: List[B]): List[B] = {
    @tailrec
    def loop(a: List[B], b: List[B]): List[B] =
      if (a.isEmpty) b
      else loop(a.tail, b.prepend(a.head))

    if (isEmpty) prefix
    else if (prefix.isEmpty) this
    else loop(prefix, this)
  }

  def concat[B >: A](xs: List[B]): List[B] =
    if (isEmpty) xs
    else tail.concat(xs).prepend(head)

  def remove[B >: A](x: B): List[B] =
    if (isEmpty) fail("empty")
    else if (x != head) List.make(head, tail.remove(x))
    else tail

  def apply(n: Int): A =
    if (isEmpty) fail("empty")
    else if (n < 0) fail("out of bound")
    else if (n == 0) head
    else tail(n - 1)

  def contains[B >: A](x: B): Boolean =
    if (isEmpty) false
    else if (x != head) tail.contains(x)
    else true

  def foreach(f: A => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  def fold[B](n: B)(op: (B, A) => B): B = {
    @tailrec
    def loop(xs: List[A], y: B): B =
      if (xs.isEmpty) y
      else loop(xs.tail, op(y, xs.head))

    loop(this, n)
  }

  def map[B](f: A => B): List[B] =
    if (isEmpty) List.empty
    else tail.map(f).prepend(f(head))

  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

  def min[B >: A](implicit ordering: Ordering[B]): B =
    if (isEmpty) fail("empty list")
    else if (tail.isEmpty) head
    else ordering.min(head, tail.min(ordering))

  def slice(from: Int, until: Int): List[A] =
    if (isEmpty || until == 0) List.empty
    else if (from == 0) tail.slice(from, until - 1).prepend(head)
    else tail.slice(from - 1, until - 1)

  def reverse: List[A] = {
    @tailrec
    def loop(s: List[A], d: List[A]): List[A] =
      if (s.isEmpty) d
      else loop(s.tail, d.prepend(s.head))

    loop(this, List.empty)
  }

  def length: Int =
    if (isEmpty) 0
    else 1 + tail.length

  def fail(msg: String) = throw new NoSuchElementException(msg)
}

case object Nil extends List[Nothing] {
  override def head: Nothing = fail("empty list")

  override def tail: List[Nothing] = fail("empty list")

  override def isEmpty: Boolean = true
}

case class Cons[A](head: A, tail: List[A]) extends List[A] {
  def isEmpty: Boolean = false
}

object List {
  def empty[A]: List[A] = Nil

  def make[A](h: A, t: List[A] = Nil): List[A] = Cons(h, t)

  def apply[A](xs: A*): List[A] = {
    var r: List[A] = List.empty

    for (x <- xs.reverse) r = r.prepend(x)
    r
  }
}

package io.loustler.heap

sealed abstract class Heap[+A] { self =>
  def min: A

  def left: Heap[A]

  def right: Heap[A]

  def size: Int

  def height: Int

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def insert[B >: A](x: B)(implicit Ord: Ordering[B]): Heap[B] = {
    import Ord._

    if (isEmpty) Heap.make(x)
    else if (left.size < math.pow(2, left.height) - 1) Heap.bubbleUp(self.min, self.left.insert(x), self.right)
    else if (right.size < math.pow(2, right.height) - 1) Heap.bubbleUp(self.min, self.left, self.right.insert(x))
    else if (right.height < left.height) Heap.bubbleUp(self.min, self.left, self.right.insert(x))
    else Heap.bubbleUp(self.min, self.left.insert(x), self.right)
  }

  def remove: Heap[A] = {
    def floatLeft(x: A, left: Heap[A], right: Heap[A]): Heap[A] = left match {
      case HeapBranch(leftMin, left2, leftRight, _, _) =>
        Heap.make(leftMin, Heap.make(x, left2, leftRight), right)

      case _ => Heap.make(x, left, right)
    }

    def floatRight(x: A, left: Heap[A], right: Heap[A]): Heap[A] = right match {
      case HeapBranch(rightMin, rightLeft, right2, _, _) =>
        Heap.make(rightMin, left, Heap.make(x, rightLeft, right2))

      case _ => Heap.make(x, left, right)
    }

    def mergeChildren(left: Heap[A], right: Heap[A]): Heap[A] =
      if (left.isEmpty && right.isEmpty) Heap.empty[A]
      else if (left.size < math.pow(2, left.height) - 1)
        floatLeft(left.min, mergeChildren(left.left, left.right), right)
      else if (right.size < math.pow(2, right.height) - 1)
        floatRight(right.min, left, mergeChildren(right.left, right.right))
      else if (right.height < left.height)
        floatLeft(left.min, mergeChildren(left.left, left.right), right)
      else floatRight(right.min, left, mergeChildren(right.left, right.right))

    def bubbleRootDown(heap: Heap[A]): Heap[A] =
      if (heap.isEmpty) Heap.empty[A]
      else Heap.bubbleDown(heap.min, heap.left, heap.right)

    if (isEmpty) fail("An empty heap")
    else bubbleRootDown(mergeChildren(left, right))
  }

  protected def fail(msg: String) = throw new NoSuchElementException(msg)
}

final case class HeapBranch[A](min: A, left: Heap[A], right: Heap[A], size: Int, height: Int) extends Heap[A] {
  override def isEmpty: Boolean = false
}

case object Leaf extends Heap[Nothing] {
  override def min: Nothing = fail("An empty heap")

  override def left: Heap[Nothing] = fail("An empty heap")

  override def right: Heap[Nothing] = fail("An empty heap")

  override def size: Int = 0

  override def height: Int = 0

  override def isEmpty: Boolean = true
}

object Heap {
  def empty[A]: Heap[A] = Leaf

  def make[A](x: A, left: Heap[A] = empty, right: Heap[A] = empty): Heap[A] =
    HeapBranch(
      x,
      left,
      right,
      left.size + right.size + 1,
      math.max(left.height, right.height) + 1
    )

  private[Heap] def bubbleDown[A](x: A, left: Heap[A], right: Heap[A]): Heap[A] = (left, right) match {
    case (_, _) => Heap.make(x, left, right)
  }

  private[Heap] def bubbleUp[A](x: A, left: Heap[A], right: Heap[A]): Heap[A] = (left, right) match {
    case (_, _) => Heap.make(x, left, right)
  }
}

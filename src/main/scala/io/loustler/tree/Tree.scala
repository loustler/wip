package io.loustler.tree

sealed abstract class Tree[+A] {
  def value: A

  def left: Tree[A]

  def right: Tree[A]

  def size: Int

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def isBalanced: Boolean = {
    def loop(t: Tree[A]): Int =
      if (t.isEmpty) 0
      else {
        val l = loop(t.left)

        if (l == -1) -1
        else {
          val r = loop(t.right)

          if (r == -1) -1
          else if (math.abs(l - r) > 1) -1
          else 1 + math.max(l, r)
        }
      }

    loop(this) != -1
  }

  def add[B >: A](x: B)(implicit OrdB: Ordering[B]): Tree[B] = {
    import OrdB._

    if (isEmpty) Tree.make(x)
    else if (x < value) Tree.make(value, left.add(x), right)
    else if (x > value) Tree.make(value, left, right.add(x))
    else this
  }

  /**
    *
    *
    * @param x
    * @param OrdB
    * @tparam B
    * @return
    */
  def remove[B >: A](x: B)(implicit OrdB: Ordering[B]): Tree[B] = {
    import OrdB._

    if (isEmpty) fail(s"Cannot find ${x} in this Tree")
    else if (x < value) Tree.make(value, left.remove(x), right) // 삭제하려는 X가 더 작을 때, 왼쪽트리에서 삭제
    else if (x > value) Tree.make(value, left, right.remove(x)) // 삭제하려는 X가 더 클 때, 오른쪽트리에서 삭제
    else { // 똑같을 때
      if (left.isEmpty && right.isEmpty) Tree.empty // 좌/우 트리가 없으면 가지고 있던 값이 사라졌으므로 Empty Tree
      else if (right.isEmpty) left                  // right가 empty 라면 left만. value는 삭제되었으므로
      else if (left.isEmpty) right // left가 emp¥라면 right만. valu는 삭제되었으므로
      else { // left, right 둘 다 있을 대
        val succ: A = right.min

        Tree.make[B](succ, left, right.remove[B](succ)(OrdB))
      }
    }
  }

  def contains[B >: A](x: B)(implicit OrdB: Ordering[B]): Boolean = {
    import OrdB._

    @scala.annotation.tailrec
    def loop(t: Tree[A], c: Option[A]): Boolean =
      if (t.isEmpty) check(c) // tree가 empty면 value 확인
      else if (x < t.value) loop(t.left, c) // x가 value보다 작으면 왼쪽 트리에서 검색
      else loop(t.right, Some(t.value)) // x <= value 에서는 오른쪽 트리에서 검색하고, 현재값 비교를 위해서 확인

    def check(c: Option[A]): Boolean = c match {
      case Some(y) if x == y => true

      case _ => false
    }

    loop(this, None)
  }

  def subtree[B >: A](x: B)(implicit OrdB: Ordering[B]): Tree[B] = {
    import OrdB._

    if (isEmpty) fail("Cannot make subtree from this tree")
    else if (x < value) left.subtree(x)
    else if (x > value) right.subtree(x)
    else this
  }

  def isSubtree[B >: A](tree: Tree[B])(implicit OrdB: Ordering[B]): Boolean = {
    def loop(left: Tree[B], right: Tree[B]): Boolean =
      if (left.isEmpty && right.isEmpty) true
      else if (left.isEmpty || right.isEmpty) false
      else left.value == right.value && loop(left.left, right.left) && loop(left.right, right.right) // left tree, right tree의 서브트리 중 일치하는 게 있는 지 확인

    loop(subtree(tree.value), tree) // 주어진 tree와 현재 tree의 서브트리가 일치하는 지 확인
  }

  def merge[B >: A](tree: Tree[B])(implicit OrdB: Ordering[B]): Tree[B] = {
    // recursive로 other에 계속 self의 값을 더함
    def loop(self: Tree[B], other: Tree[B]): Tree[B] =
      if (self.isEmpty) other
      else loop(self.right, loop(self.left, other.add(self.value)))

    loop(this, tree)
  }

  // 왼쪽 트리에서부터 쭉 진행
  def foreach(f: A => Unit): Unit =
    if (nonEmpty) {
      left.foreach(f)
      f(value)
      right.foreach(f)
    }

  def fold[B](n: B)(op: (B, A) => B): B = {
    // 왼쪽 트리부터 순차적으로 값을 가지고 와서 op에 적용
    def loop(tree: Tree[A], x: B): B =
      if (tree.isEmpty) x
      else loop(tree.right, op(loop(tree.left, x), tree.value))

    loop(this, n)
  }

  def map[B >: A](f: A => B)(implicit OrdB: Ordering[B]): Tree[B] =
    if (isEmpty) Tree.empty
    else Tree.make(f(value), left.map(f), right.map(f))

  def invert[B >: A](implicit num: Numeric[B]): Tree[B] =
    if (isEmpty) Tree.empty
    else Tree.make(num.negate(value), left.invert(num), right.invert(num))

  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

  def min: A = {
    @scala.annotation.tailrec
    def loop(tree: Tree[A], m: A): A =
      if (tree.isEmpty) m
      else loop(tree.left, tree.value)

    loop(left, value)
  }

  def max: A = {
    def loop(tree: Tree[A], m: A): A =
      if (tree.isEmpty) m
      else loop(tree.right, tree.value)

    if (isEmpty) fail("An empty tree")
    else loop(right, value)
  }

  /**
    * Left tree의 height부터 구하고 right ree의 height를 recursive로 계산
    *
    * @return
    */
  def height: Int =
    if (isEmpty) 0
    else 1 + math.max(left.height, right.height)

  def depth[B >: A](x: B)(implicit OrdB: Ordering[B]): Int = {
    import OrdB._

    if (isEmpty) fail(s"Cannot find $x depth in this tree")
    else if (x < value) 1 + left.depth(x)
    else if (x > value) 1 + right.depth(x)
    else 0
  }

  /**
    * 순위를 찾을 때 Left Tree에서부터 검색.
    *
    * Left Tree가 가장 작은 숫자를 가지고 있으므로
    *
    * Right Tree에서 검색할 때에는 Left Tree들의 rank를 그 개수인 size만큼 plus를 해주고 거기에 + 1 을 해줘야 됨(현재값 포함ㅖ
    *
    * 현재 값과 같다면 Left Tree size
    *
    * @param x
    * @param OrdB
    * @tparam B
    * @return
    */
  def rank[B >: A](x: B)(implicit OrdB: Ordering[B]): Int = {
    import OrdB._

    if (isEmpty) 0
    else if (x < value) left.rank(x)
    else if (x > value) 1 + left.size + right.rank(x)
    else left.size
  }

  def path[B >: A](x: B)(implicit OrdB: Ordering[B]): List[Tree[B]] = {
    import OrdB._

    if (isEmpty) fail(s"Cannot find $x path in This tree")
    else if (x < value) this :: left.path(x)
    else if (x > value) this :: right.path(x)
    else List(this)
  }

  /**
    * Root Node로부터 목표 [[x]]까지의 경로 찾기
    *
    * @param x
    * @param OrdB
    * @tparam B
    * @return
    */
  def trace[B >: A](x: B)(implicit OrdB: Ordering[B]): List[A] = {
    import OrdB._

    if (isEmpty) fail(s"Cannot find $x trace in this tree")
    else if (x > value) this.value :: left.trace(x)
    else if (x < value) this.value :: right.trace(x)
    else List(this.value)
  }

  /**
    * x번째 요소 검색
    *
    * @param x
    * @return
    */
  def apply(x: Int): A =
    if (isEmpty) fail(s"Tree doesn't contain a ${x}th element.")
    else {
      val size = left.size
      if (x < size) left(x)
      else if (x > size) right(x - size - 1)
      else value
    }

  protected def fail(msg: String) = throw new NoSuchElementException(msg)
}

case object Leaf extends Tree[Nothing] {
  override def value: Nothing = fail("empty tree")

  override def left: Tree[Nothing] = fail("empty tree")

  override def right: Tree[Nothing] = fail("empty tree")

  override def size: Int = 0

  override def isEmpty: Boolean = true
}

final case class Branch[+A](
  value: A,
  left: Tree[A],
  right: Tree[A],
  size: Int
) extends Tree[A] {
  override def isEmpty: Boolean = false
}

object Tree {
  def empty[A]: Tree[A] = Leaf

  def make[A](x: A, left: Tree[A] = Tree.empty, right: Tree[A] = Tree.empty): Tree[A] =
    Branch(x, left, right, left.size + right.size + 1)

  def apply[A](x: A*)(implicit Ord: Ordering[A]): Tree[A] = {
    var tree = Tree.empty[A]

    for (v <- x) tree = tree.add(v)
    tree
  }

}

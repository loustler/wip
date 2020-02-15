package io.loustler.tree

sealed trait Color
case object Red   extends Color
case object Black extends Color

sealed abstract class RedBlackTree[+A] {
  def color: Color

  def value: A

  def left: RedBlackTree[A]

  def right: RedBlackTree[A]

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def add[B >: A](x: B)(implicit OrdB: Ordering[B]): RedBlackTree[B] = {
    import OrdB._

    def balanceAdd(tree: RedBlackTree[A]): RedBlackTree[B] =
      if (tree.isEmpty) RedBlackTree.make(Red, x)
      else if (x < tree.value) balanceLeft(tree.color, tree.value, balanceAdd(tree.left), tree.right)
      else if (x > tree.value) balanceRight(tree.color, tree.value, tree.left, balanceAdd(tree.right))
      else tree

    def balanceLeft(color: Color, x: A, left: RedBlackTree[B], right: RedBlackTree[A]) =
      (color, left, right) match {

        /**
          * Left:
          * Before =>
          *                            Root
          *                           /
          *                Left Root(Black, x)
          *                  /        \
          *          Branch(Red, F)   H
          *           /        \
          *    Branch(Red, D)  G
          *        /    \
          *       B     E
          *      /  \
          *     A   C
          *
          * After  =>
          *                       Root
          *                      /
          *                Left Root(Red, D)
          *              /                 \
          *      Branch(Black, B)     Branch(Black, x)
          *          /     \             /      \
          *         A       C           G       H
          *
          */
        case (Black, RbBranch(Red, d, RbBranch(Red, b, a, c), g), h) =>
          RedBlackTree.make(
            Red,
            d,
            RedBlackTree.make(Black, b, a, c), // Recoloring
            RedBlackTree.make(Black, x, g, h) // Restructuring
          )

        /**
          * Left:
          * Before =>
          *                                 Root
          *                                /
          *                    Left Root(Black, x)
          *                     /                \
          *           Branch(Red, B)            F
          *            /       \
          *         A     Branch(Red, D)
          *                 /       \
          *               C        E
          *
          *
          * After =>
          *                       Root
          *                       /
          *                  Left Root(Red, D)
          *                 /          \
          *        Branch(Black, B)    Branch(Black, x)
          *       /            \        /           \
          *      A             C       E             F
          *
          */
        case (
            Black,
            RbBranch(Red, b, a, RbBranch(Red, d, c, e)),
            f
            ) =>
          RedBlackTree.make(
            Red,
            d,
            RedBlackTree.make(Black, b, a, c), // Restructuring
            RedBlackTree.make(Black, x, e, f) // Recoloring
          )

        case _ => RedBlackTree.make(color, x, left, right)
      }

    def balanceRight(color: Color, x: A, left: RedBlackTree[A], right: RedBlackTree[B]) =
      (color, left, right) match {

        /**
          * Right:
          * Before =>
          *       Root
          *          \
          *        Right Root(Black, x)
          *                  /     \
          *               A    Branch(Red, C)
          *                     /         \
          *                    B      Branch(Red, E)
          *                              /     \
          *                             D       F
          *
          * After =>
          *       Root
          *           \
          *           Left Root(Red, C)
          *              /          \
          *       Branch(Black, x)  Branch(Black, E)
          *          /     \            /    \
          *         A      B          D       F
          *
          */
        case (
            Black,
            a,
            RbBranch(Red, c, b, RbBranch(Red, e, d, f))
            ) =>
          RedBlackTree.make(
            Red,
            c,
            RedBlackTree.make(Black, x, a, b),
            RedBlackTree.make(Black, e, d, f)
          )

        /**
          * Right:
          * Before =>
          *          Root
          *             \
          *            Right Root(Black, x)
          *             /       \
          *            A      Branch(Red, E)
          *                      /         \
          *               Branch(Red, C)    F
          *                 /        \
          *                B         D
          *
          * After =>
          *          Root
          *            \
          *           Right Root(Red, C)
          *          /             \
          *     Branch(Black, x)  Branch(Black, E)
          *     /          \         /       \
          *   A           B         D         F
          *
          */
        case (
            Black,
            a,
            RbBranch(Red, e, RbBranch(Red, c, b, d), f)
            ) =>
          RedBlackTree.make(
            Red,
            c,
            RedBlackTree.make(Black, x, a, b),
            RedBlackTree.make(Black, e, d, f)
          )

        case _ => RedBlackTree.make(color, x, left, right)
      }

    def blacken(tree: RedBlackTree[B]): RedBlackTree[B] = RedBlackTree.make(Black, tree.value, tree.left, tree.right)

    blacken(balanceAdd(this))
  }

  def height: Int = if (isEmpty) 0 else math.max(left.height, right.height) + 1

  protected def fail(msg: String) = throw new NoSuchElementException(msg)
}

case object RbLeaf extends RedBlackTree[Nothing] {
  override def color: Color = Black

  override def value: Nothing = fail("An empty tree")

  override def left: RedBlackTree[Nothing] = fail("An empty tree")

  override def right: RedBlackTree[Nothing] = fail("An empty tree")

  override def isEmpty: Boolean = true
}

final case class RbBranch[A](
  color: Color,
  value: A,
  left: RedBlackTree[A],
  right: RedBlackTree[A]
) extends RedBlackTree[A] {
  override def isEmpty: Boolean = false
}

object RedBlackTree {
  def empty[A]: RedBlackTree[A] = RbLeaf

  def make[A](
    color: Color,
    x: A,
    left: RedBlackTree[A] = RedBlackTree.empty[A],
    right: RedBlackTree[A] = RedBlackTree.empty[A]
  ): RedBlackTree[A] = RbBranch[A](color, x, left, right)

  def apply[A](xs: A*)(implicit Ord: Ordering[A]): RedBlackTree[A] = {
    var tree = RedBlackTree.empty[A]

    for (x <- xs) tree = tree.add(x)

    tree
  }
}

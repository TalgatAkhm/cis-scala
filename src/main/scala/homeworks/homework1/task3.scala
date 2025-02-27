package homeworks.homework1

/**
 * Реализуйте методы forall, exists, size, take, foldLeft.
 */
sealed trait BinaryTree {
  def insert(newValue: Int): BinaryTree =
    this match {
      case Branch(value, left, right) if newValue <= value =>
        Branch(value, left.insert(newValue), right)
      case Branch(value, left, right) if value < newValue =>
        Branch(value, left, right.insert(newValue))
      case Leaf =>
        Branch(newValue, Leaf, Leaf)
    }

  def contains(newValue: Int): Boolean =
    this match {
      case Branch(value, left, _) if newValue < value =>
        left.contains(newValue)
      case Branch(value, _, right) if value < newValue =>
        right.contains(newValue)
      case Branch(value, _, _) if value == newValue =>
        true
      case Leaf =>
        false
    }

  def dump(depth: Int = 0): Unit =
    this match {
      case Branch(value, left, right) =>
        right.dump(depth + 1)
        println(" " * depth + value)
        left.dump(depth + 1)
      case _ =>
    }

  /**
   * @param p предикат
   * @return true, если все элементы в дереве удовлетворяют предикату p, иначе false
   */
  def forall(p: Int => Boolean): Boolean =
    this match {
      case Branch(value, left, right) =>
        p(value) && left.forall(p) && right.forall(p)
      case _ =>
        true
    }

  /**
   * @param p предикат
   * @return true, если в дереве существует элемент, удовлетворяющий предикату p, иначе false
   */
  def exists(p: Int => Boolean): Boolean =
    this match {
      case Branch(value, left, right) =>
        p(value) || left.exists(p) || right.exists(p)
      case _ =>
        false
    }

  /**
   * @return число элементов в дереве
   */
  def size: Int =
    this match {
      case Branch(value, left, right) =>
        left.size + 1 + right.size
      case _ =>
        0
    }

  /**
   * Возвращает дерево из первых n элементов этого дерева. Если размер дерева меньше n, возвращает this
   *
   * @param n число элементов, которые нужно вернуть
   * @return новое дерево из первых n элементов дерева. Желательно, переиспользует структуру существующего дерева
   */
  def take(n: Int): BinaryTree = {
    val size = this.size
    if (size <= n)
      this
    else {
      this match {
        case Branch(value, left, right) =>
          val leftPart = left.take(n)
          val leftSize = leftPart.size
          if (n > leftSize) Branch(value, leftPart, right.take(n - leftSize - 1)) else leftPart
      }
    }
  }

  /**
   * Применяет оператор op ко всем элементам дерева по порядку, начиная с z.
   * Например, для дерева
   *     5
   *    / \
   *   4  8
   *     / \
   *    6  10
   * результат будет эквивалентен вызову
   *   op(op(op(op(op(z, 4), 5), 6), 8), 10)
   *
   * @param z начальное значение
   * @param op оператор
   * @tparam B тип начального значения и результата
   * @return результат применения оператора ко всем элементам дерева
   */
  def foldLeft[B](z: B)(op: (B, Int) => B): B =
    this match {
      case Branch(value, left, right) =>
        val accept = (operand: B, part: BinaryTree) => part.foldLeft(operand)(op)
        accept(
          op(accept(z, left), value),
          right
        )
      case _ => z
    }
}

final case class Branch(value: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree

case object Leaf extends BinaryTree

object task3 extends App {
  //   5
  //  / \
  // 4  8
  //   / \
  //  6  10
  val tree =
    Leaf
      .insert(5)
      .insert(4)
      .insert(8)
      .insert(10)
      .insert(6)

  println(
    tree.forall(_ > 20),
    tree.forall(_ > 4),
    tree.forall(_ > 0)
  )
  // (false, false, true)

  println(
    tree.forall(_ > 20),
    tree.exists(_ > 4),
    tree.exists(_ > 0)
  )
  // (false, true, true)

  println(tree.take(3))
  //   5
  //  / \
  // 4  6

  println(tree.foldLeft("")(_ + _))
  // 456810
}

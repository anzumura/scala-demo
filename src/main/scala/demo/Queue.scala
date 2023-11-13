package demo

class Queue[T] private(val leading: List[T], val trailing: List[T]) {
  def head: T = mirror.leading.head

  def tail: Queue[T] = {
    val q = mirror
    new Queue(q.leading.tail, q.trailing)
  }

  def enqueue(x: T): Queue[T] = new Queue(leading, x :: trailing)

  override def toString: String = {
    (leading ::: trailing.reverse).mkString("Queue(", ",", ")")
  }

  override def equals(that: Any): Boolean = that match {
    case q: Queue[_] => (this eq q) ||
      leading ::: trailing.reverse == q.leading ::: q.trailing.reverse
    case _ => false
  }

  override def hashCode: Int = (leading ::: trailing.reverse).hashCode

  private def mirror = if (leading.isEmpty)
    new Queue(trailing.reverse, Nil)
  else
    this
}

object Queue {
  def apply[T](xs: T*): Queue[T] = new Queue(xs.toList, Nil)

  def empty[T]: Queue[T] = new Queue[T](Nil, Nil)
}
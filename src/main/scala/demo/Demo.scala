package demo

import scala.collection.mutable

object Demo {
  def msort[T](xs: List[T])(less: (T, T) => Boolean): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (_, Nil) => xs
      case (Nil, _) => ys
      case (x :: xs1, y :: ys1) =>
        if (less(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
    }

    xs match {
      case Nil | _ :: Nil => xs
      case _ =>
        val (x, y) = xs.splitAt(xs.size / 2)
        merge(msort(x)(less), msort(y)(less))
    }
  }

  def countWords(s: String): Map[String, Int] = {
    val result = mutable.Map[String, Int]().withDefaultValue(0)
    s.split("[ !.,?]+").map(_.toLowerCase).foreach(result(_) += 1)
    result.toMap
  }
}

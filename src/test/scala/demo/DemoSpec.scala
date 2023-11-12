package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

class DemoSpec extends AnyFlatSpec with Matchers {
  import Demo._

  private val gen = new Random(System.currentTimeMillis)

  private def anyList(len: Int = -1) = {
    Seq.fill(if (len < 0) gen.nextInt(100) else len)(gen.nextInt()).toList
  }

  behavior of "Demo"

  it should "sort empty list" in {
    msort(List.empty[Int])(_ < _) shouldBe Nil
  }

  it should "sort list with single element" in {
    msort(List(2))(_ < _) shouldBe List(2)
  }

  it should "sort list with negative and positive numbers" in {
    msort(List(-2, 7, 9, -5, 4, 0))(_ < _) shouldBe List(-5, -2, 0, 4, 7, 9)
  }

  it should "sort list with duplicate numbers" in {
    msort(List(4, 1, -5, 4, 0))(_ < _) shouldBe List(-5, 0, 1, 4, 4)
  }

  it should "sort large list of random numbers" in {
    val l = anyList()
    msort(l)(_ < _) shouldBe l.sorted
  }
}

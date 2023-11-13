package demo

import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QueueSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  behavior of "Queue"

  private var q = Queue.empty[Int]

  private def enqueue(x: Int) = {
    q = q.enqueue(x)
    q
  }

  override protected def beforeEach(): Unit = {
    q = Queue.empty[Int]
  }

  it should "construct from a list of values" in {
    q = Queue(2, 4, 6, 1, 3)
    q.toString shouldBe "Queue(2,4,6,1,3)"
  }

  it should "support enqueue, head and tail" in {
    q.toString shouldBe "Queue()"
    enqueue(4) shouldBe Queue(4)
    q.head shouldBe 4
    q.tail shouldBe Queue.empty[Int]
    enqueue(6)
    enqueue(2)
    q.head shouldBe 4
    q.tail shouldBe Queue(6, 2)
  }
}

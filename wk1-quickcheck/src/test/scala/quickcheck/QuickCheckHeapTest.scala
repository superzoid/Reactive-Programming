package quickcheck

import org.scalatest.{FlatSpec, Matchers}
import quickcheck.QuickCheckBinomialHeap._

import scala.annotation.tailrec

class QuickCheckHeapTest extends FlatSpec with Matchers{

  "isSorted" should "be true for empty Heap" in {
    isSorted(QuickCheckBinomialHeap.empty) shouldBe true
  }

  it should "be true for a sorted Heap" in {
    val sortedHeap = insert(1, insert(2, insert(3, QuickCheckBinomialHeap.empty)))
    isSorted(sortedHeap) shouldBe true
  }

  def isSorted(heap: H): Boolean = {

    @tailrec
    def go(prev: Int, h: H): Boolean = {
      if(isEmpty(h)) return true
      if(prev > findMin(h)) {
        return false
      } else {
        return go(findMin(h), deleteMin(h))
      }
    }

    if(isEmpty(heap)) return true
    go(findMin(heap),deleteMin(heap))

  }

}

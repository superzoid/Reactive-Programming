package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property(" deleteMin should not give second lowest") = forAll { (a: Int, b: Int, c: Int) =>
    val sortedInts = List(a,b,c).sorted
    val lowest = sortedInts.head
    val secondLowest = sortedInts.tail.head

    val h = insert(c, insert(b, insert(a, empty)))
    val min = findMin(h)

    if( min != lowest ){
      println (s"error condition : $min > $lowest")
    }

    val h2 = deleteMin(h)
    findMin(h2) == secondLowest
  }

  property(" with one element returns only value for findMin") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property(" with one element that has its element deleted is empty") = forAll { a: Int =>
    val h = insert(a, empty)
    val shouldBeEmpty = deleteMin(h)
    isEmpty(shouldBeEmpty)
  }

  property(" is always sorted") = forAll {
    h: H => isSorted(h)
  }

  property(" meld joins correctly") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)

    val melded = meld(h1, h2)

    findMin(melded) == Math.min(min1, min2)
  }

  property(" finds min in melded") = forAll { (n: Int, m: Int) =>
    val heap1 = insert(n, empty)
    val heap2 = insert(m, empty)
    val melded = meld(heap1, heap2)
    val min = findMin(melded)
    if (n <= m)
      min == n
    else
      min == m
  }

  def isSorted(heap: H): Boolean = {

    @tailrec
    def loop(prev: Int, h: H): Boolean = {
      if(isEmpty(h)) return true
      if(prev > findMin(h)) false
      else loop(findMin(h), deleteMin(h))
    }

    if(isEmpty(heap)) return true
    loop(findMin(heap),deleteMin(heap))

  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}

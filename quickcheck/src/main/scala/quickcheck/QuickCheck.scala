package quickcheck

import org.scalacheck._
import Arbitrary.{arbitrary, _}
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
    element <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(element, heap))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfTwo") = forAll { (a: Int, b: Int) =>
    val min = if (a < b) a else b
    findMin(insert(b, insert(a, empty))) == min
  }

  property("emptyWhenDeleteAfterOneInsert") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("orderedSequence") = forAll { list: List[A] =>
    val ordering = list.sorted
    val heap = insert(empty, list)
    sequenceFromHeap(heap) == ordering
  }

  property("minimumOfMelding") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = if (min1 < min2) min1 else min2
    val meldedHeap = meld(h1, h2)
    findMin(meldedHeap) == min
  }

  def sequenceFromHeap(h: H): List[A] = if (isEmpty(h)) {
    List()
  } else {
    findMin(h) :: sequenceFromHeap(deleteMin(h))
  }

  def insert(h: H, list: List[A]): H = list match {
    case x :: xs => insert(insert(x, h), xs)
    case Nil => h
  }
}

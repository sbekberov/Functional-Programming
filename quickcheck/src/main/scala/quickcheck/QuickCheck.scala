package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      a <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  def equality (h1: H, h2: H): Boolean = {
    if (isEmpty(h1) && isEmpty(h2)) {
      true
    } else if (isEmpty(h1) != isEmpty(h2) || findMin(h1) != findMin(h2)) {
      false
    } else {
      equality(deleteMin(h1), deleteMin(h2))
    }
  }

  property("insertTwoElemsIntoEmptyH") = forAll({
    (elem1: Int, elem2: Int) => {
      val h = insert(elem2, insert(elem1, empty))
      val min = if (elem1 < elem2) elem1 else elem2
      findMin(h) == min
    }
  })

  property("insertElemDeleteElem") = forAll({
    (elem: Int) => {
      val h = insert(elem, empty)
      isEmpty(deleteMin(h))
    }
  })

  property("findMinRemoveMin") = forAll({
    (h: H) => {
      def removeMinElem(curr: H): List[Int] = {
        if (isEmpty(curr)) Nil
        else findMin(curr) +: removeMinElem(deleteMin(curr))
      }

      val sortedH = removeMinElem(h)
      sortedH == sortedH.sorted
    }
  })

  property("unionHeapsFindMin") = forAll({
    (h1: H, h2: H) => {
      val minH1 = findMin(h1)
      val minH2 = findMin(h2)
      val unionH = meld(h1, h2)
      val minUnionH = findMin(unionH)
      minUnionH == minH1 || minUnionH == minH2
    }
  })

  property("insertDeleteIntoEmptyHeap") = forAll({
    (elem1: Int, elem2: Int) => {
      val h = insert(elem2, insert(elem1, empty))
      val h2 = deleteMin(h)
      isEmpty(deleteMin(h2))
    }
  })

  property("insertNotMin") = forAll({
    (h: H) => {
      if (isEmpty(h)) {
        true
      } else {
        val min = findMin(h)
        val next = if (min == Int.MaxValue) Int.MaxValue else min + 1
        min == findMin(insert(next, h))
      }
    }
  })

  property("compareTwoHeaps") = forAll({
    (h1: H, h2: H) => {
      if (isEmpty(h1)) {
        equality(meld(h1, h2), meld(h1, h2))
      } else {
        val min = findMin(h1)
        equality(meld(h1, h2), meld(deleteMin(h1), insert(min, h2)))
      }
    }
  })

  property("unionList") = forAll({
    (h1: H, h2: H) => {
      equality(meld(h1, h2), meld(h2, h1))
    }
  })

  property("isElemInHeap") = forAll({
    (h: H, elem: Int) => {
      @tailrec
      def find(h: H, elem: Int): Boolean = {
        if (isEmpty(h)) false
        else if(findMin(h) == elem) true
        else find(deleteMin(h),elem)
      }
      find(insert(elem, h), elem)
    }
  })
}

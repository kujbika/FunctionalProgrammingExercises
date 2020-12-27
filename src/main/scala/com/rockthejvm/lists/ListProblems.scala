package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.util.Random.nextInt

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def headOption: Option[T]

  /*
   * Easy problems
   */
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def take(n: Int): RList[T]

  def reverse: RList[T]

  def ++[R >: T](other: RList[R]): RList[R]

  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /*
   * Medium problems
   */
  // run-length encoding
  def rle: RList[(T, Int)]

  def duplicateEach(number: Int): RList[T]

  def rotate(number: Int): RList[T]

  def sample(number: Int): RList[T]

  def reduce[A1 >: T](f: (A1, A1) => A1): A1

  // the output could be a tuple, but I wanted to stay at my abstraction
  def zip[S >: T](other: RList[S]): RList[RList[S]]

  def skip(n: Int): RList[T] 


  /*
   * Hard problems
   */

  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]
  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
  def quickSort[S >: T](ordering: Ordering[S]): RList[S]

}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def convertToRListTailrec(remaining: Iterable[T], soFar: RList[T]): RList[T] = {
      if (remaining.isEmpty) soFar
      else convertToRListTailrec(remaining.tail, remaining.head :: soFar)
    }
    convertToRListTailrec(iterable, RNil).reverse
  }

  def apply[T](args: T*): RList[T] = {
    RList.from(args.toList)
  }

  implicit class wrapper[T](elem: RList[RList[T]]) {
    def flatten: RList[T] = elem.flatMap(x => x)
  }
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def headOption: Option[Nothing] = None 

  override def toString: String = ""

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def take(n: Int): RList[Nothing] = RNil

  override def reverse: RList[Nothing] = RNil

  override def ++[R](other: RList[R]): RList[R] = other

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(number: Int): RList[Nothing] = RNil

  override def rotate(number: Int): RList[Nothing] = RNil

  override def sample(number: Int): RList[Nothing] = RNil

  override def reduce[A1](f: (A1, A1) => A1): A1 = throw new NoSuchElementException

  override def skip(n: Int): RList[Nothing] = RNil

  override def zip[S](other: RList[S]): RList[RList[S]] = RNil

  override def insertionSort[S](ordering: Ordering[S]): RList[S] = RNil

  override def mergeSort[S](ordering: Ordering[S]): RList[S] = RNil

  override def quickSort[S](ordering: Ordering[S]): RList[S] = RNil
  
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def headOption: Option[T] = Some(head) 

  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
    }
    s"[${toStringTailRec(this, "")}]" 
  }

  override def apply(index: Int): T = {

    @tailrec
    def recursiveGet(remaining: RList[T], currentIndex: Int): T = {
      // complexity: O(n) at worst
      if (currentIndex == 0) remaining.head
      else recursiveGet(remaining.tail, currentIndex - 1)
    }

    recursiveGet(this, index)
  }

  override def length: Int = {

    @tailrec
    def lengthHelper(remaining: RList[T], acc: Int): Int = {
      if (remaining.isEmpty) acc
      else lengthHelper(remaining.tail, acc + 1)
    }

    lengthHelper(this, 0)
  }

  override def take(n: Int): RList[T] = {
    @tailrec
    def helper(remaining: RList[T], index: Int, soFar: RList[T]): RList[T] = {
      if (remaining.tail.isEmpty) remaining.head :: soFar
      else if (index >= n) soFar
      else helper(remaining.tail, index + 1, remaining.head :: soFar)
    }
    helper(this, 0, RNil).reverse
  }

  /*
   * Easy problems
   */
  override def reverse: RList[T] = {
    @tailrec
    def reverseHelper(remaining: RList[T], soFar: RList[T]): RList[T] = {
      if (remaining.isEmpty) soFar
      else reverseHelper(remaining.tail, soFar.::(remaining.head))
    }

    reverseHelper(this, RNil)
  }

  override def ++[R >: T](other: RList[R]): RList[R] = {
    // complexity is O(m + n)
    @tailrec
    def concat(remaining: RList[R], soFar: RList[R]): RList[R] = {
      if (remaining.isEmpty) soFar
      else concat(remaining.tail, remaining.head :: soFar)
    }
    concat(this.reverse, other) 
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeHelper(listBefore: RList[T], listAfter: RList[T], position: Int = 0): RList[T] = {
      if (position == index) listBefore ++ listAfter
      else removeHelper((this(position) :: listBefore.reverse).reverse, listAfter.tail, position + 1)
    } 
    if (index >= this.length) this
    removeHelper(RNil, this.tail)
  }

  override def map[S](f: T => S): RList[S] = {
    // this is the easy way
    // but it is not efficient
    // this.flatMap(x => RList(f(x)))
    @tailrec
    def helper(remaining: RList[T], soFar: RList[S]): RList[S] = {
      if (remaining.isEmpty) soFar
      else helper(remaining.tail, f(remaining.head) :: soFar)
    }
    helper(this, RNil).reverse
  }
  
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    // this is a naive implementation
    // complexity: 
    // let z = sum of all lengths of f(x)
    // o(z^2) - enormously big!
    @tailrec
    def naiveFlatMap(remaining: RList[T], soFar: RList[S]): RList[S] = {
      if (remaining.isEmpty) soFar
      else naiveFlatMap(remaining.tail, soFar ++ f(remaining.head))
    }

    /*
     * More efficient flatmap
     * this is so much faster
     */
    @tailrec
    def elegantFlatMap(remaining: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty) accumulator.reduce((x, y) => y ++ x)
      else elegantFlatMap(remaining.tail, f(remaining.head) :: accumulator)
    }

    // naiveFlatMap(this, RNil)
    elegantFlatMap(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterHelper(remaining: RList[T], soFar: RList[T]): RList[T] = {
      if (remaining.isEmpty) soFar
      else filterHelper(remaining.tail, if (f(remaining.head)) remaining.head :: soFar else soFar)
    }

    filterHelper(this, RNil).reverse
  }

  /*
   * Medium problems
   */
  override def rle: RList[(T, Int)] = {
    @tailrec
    def helper(remaining: RList[T], foundSnippet: RList[T], soFar: RList[(T, Int)]): RList[(T, Int)] = {
      // if the consecutive duplicate snippet found:
      // list => RList((list.head, list.length))
      // if two such snippets found: ++
      if (remaining.isEmpty) soFar ++ RList((foundSnippet.head, foundSnippet.length))
      else {
        if (remaining.head == foundSnippet.head) helper(remaining.tail, remaining.head :: foundSnippet, soFar)
        else helper(remaining.tail, RList(remaining.head), soFar ++ RList((foundSnippet.head, foundSnippet.length)))
      }
    }
    helper(this.tail, RList(head), RNil)
  }

  override def duplicateEach(number: Int): RList[T] =
    this.flatMap(element => RList.from(1 to number).flatMap(x => RList(element)))
  
  override def rotate(number: Int): RList[T] = {
    @tailrec
    def helper(remaining: RList[T], soFar: RList[T], index: Int): RList[T] = {
      if (index == 0) soFar
      else if (remaining.isEmpty) helper(this.tail, soFar.tail ++ RList(head), index - 1)
      else helper(remaining.tail, soFar.tail ++ RList(remaining.head), index - 1)
    }
    helper(this, this, number)
  }

  override def sample(number: Int): RList[T] = 
    RList.from(0 to number).map(_ => this(nextInt(this.length)))

  override def reduce[A1 >: T](f: (A1, A1) => A1): A1 = {

    @tailrec
    def reduceNaive(remaining: RList[T], soFar: A1): A1 = {
      if (remaining.length == 1) remaining.head
      else if (remaining.tail.isEmpty) f(soFar, remaining.head)
      else reduceNaive(remaining.tail, f(soFar, remaining.head))
    }


    @tailrec
    def reduceDivideAndConquer(remaining: RList[A1]): A1 = {
      if (remaining.length == 1) remaining.head
      else if (remaining.length == 2) f(remaining.head, remaining(1))
      else if (remaining.length % 2 == 1) reduceDivideAndConquer(f(remaining.head, remaining(1)) :: remaining.tail.tail)
      else reduceDivideAndConquer(remaining.zip(remaining.tail).skip(2).map(listOfTwo => f(listOfTwo.head, listOfTwo(1))))
    } 
    // if conquer and division is wanted, use the below line
    reduceDivideAndConquer(this)
    
    // if no conquer and division is applied, comment out the next line
    // reduceNaive(this.tail, this.head)
  }

  override def skip(n: Int): RList[T] = {
    @tailrec
    def helper(remaining: RList[T], soFar: RList[T], index: Int): RList[T] = {
      if (remaining.tail.isEmpty && (index) % n == 0) remaining.head ::soFar
      else if (remaining.tail.isEmpty) soFar
      else if ((index) % n == 0) helper(remaining.tail, remaining.head :: soFar, index + 1)
      else helper(remaining.tail, soFar, index + 1)
    }
    helper(this, RNil, 0).reverse
  }

  override def zip[S >: T](other: RList[S]): RList[RList[S]] = {
    @tailrec
    def zipHelper(remaining: RList[T], other: RList[S],soFar: RList[RList[S]], index: Int): RList[RList[S]] = {
      if (remaining.isEmpty || other.isEmpty) soFar
      else zipHelper(remaining.tail, other.tail, RList(remaining.head, other.head) :: soFar, index + 1)
    }
    zipHelper(this, other, RNil, 0).reverse
  }

  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    // after is insertionSort
    def insertSorted(elem: T, before: RList[S], after: RList[S]): RList[S] = {
      // complexity: O(N)
      if (after.isEmpty || ordering.lteq(elem, after.head)) before.reverse ++ (elem :: after)
      else insertSorted(elem, after.head :: before, after.tail)
    }

    @tailrec
    def insertionSortTailrec(remaining: RList[T], soFar: RList[S]): RList[S] = {
      // Complexitx: O(N^2)
      if (remaining.isEmpty) soFar
      else insertionSortTailrec(remaining.tail, insertSorted(remaining.head, RNil, soFar))
    }
    insertionSortTailrec(this, RNil)
  }

  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {

    // merge two sorted lists
    def mergeTwoHalves(a: RList[S], b: RList[S]): RList[S] = {
      @tailrec
      def helper(a: RList[S], b: RList[S], soFar: RList[S]): RList[S] = {
        if (a.isEmpty && b.isEmpty) soFar
        else if (a.isEmpty) b.reverse ++ soFar
        else if (b.isEmpty) a.reverse ++ soFar
        else if (ordering.lteq(a.head, b.head)) helper(a.tail, b, a.head :: soFar)
        else helper(a, b.tail, b.head :: soFar)
      }
      helper(a, b, RNil).reverse
    }

    // this only makes sense if the conquer and division reduce is used!
    // if thats not the case, its just an insertionSort
    this.map(RList(_)).reduce(mergeTwoHalves)
  }

  override def quickSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def pivotPartionioner(elem: S, before: RList[S], after: RList[S], accumulator: RList[S]): RList[RList[S]] = {
      if (after.isEmpty && accumulator.isEmpty) RList(before, RList(elem))
      else if (after.isEmpty && before.isEmpty) RList(RList(elem), accumulator)
      else if (after.isEmpty) RList(before, RList(elem), accumulator)
      else if (ordering.lteq(after.head, elem)) pivotPartionioner(elem, after.head :: before, after.tail, accumulator)
      else pivotPartionioner(elem, before, after.tail, after.head :: accumulator)
    }

    // this is the same merging method as in merge sort
    def mergeTwoHalves(a: RList[S], b: RList[S]): RList[S] = {
      @tailrec
      def helper(a: RList[S], b: RList[S], soFar: RList[S]): RList[S] = {
        if (a.isEmpty && b.isEmpty) soFar
        else if (a.isEmpty) b.reverse ++ soFar
        else if (b.isEmpty) a.reverse ++ soFar
        else if (ordering.lteq(a.head, b.head)) helper(a.tail, b, a.head :: soFar)
        else helper(a, b.tail, b.head :: soFar)
      }
      helper(a, b, RNil).reverse
    }

    @tailrec
    def quickSortTailRec(remaining: RList[RList[S]], soFar: RList[S]): RList[S] = {
      if (remaining.filter(_.length != 1).isEmpty) mergeTwoHalves(soFar, remaining.flatten)
      else quickSortTailRec(remaining.filter(_.length != 1)
        .flatMap(list => pivotPartionioner(list.head, RNil, list.tail, RNil)),
        mergeTwoHalves(soFar, remaining.filter(_.length == 1).flatten))
    }
    quickSortTailRec(RList(this), RNil)
  }
}

object ListProblems extends App {

  val aSmallList: RList[Int] = ::(1, ::(2, ::(3, RNil)))
  val aLargeList: RList[Int] = RList.from(1 to 10000)
  val theSameList: RList[Int] = 1 :: 2 :: 3 :: RNil

  def testEasyProblems(): Unit = {
    println(theSameList)

    println(theSameList(2))
    println(aLargeList(8435))
    
    println(s"the length of $theSameList is ${theSameList.length}")
    println(s"the length of the large list is ${aLargeList.length}")

    println(theSameList.reverse)

    println(aSmallList ++ (4 :: 5 :: RNil))

    // println(aLargeList.removeAt(48))

    // map, flatMap, filter
    println(s"_ * 2 on $aSmallList is ${aSmallList.map(_ * 2)}")
    println(s"RList(x, x+1) on $aSmallList is ${aSmallList.flatMap(x => RList(x, x + 1))}")
    println(s"_ %2 on $aSmallList is ${aSmallList.filter(_ % 2 != 0)}")

    // flatMap and filter on a big ass RList
    val currentTime = System.currentTimeMillis()
    aLargeList.flatMap(x => RList(x, x + 1))
    val nowTime = System.currentTimeMillis()
    println(nowTime - currentTime)
  }

  def testMediumProblems(): Unit = {
    // run-length encoding
    val b = RList(1,1,1,2,3,3,3,3,3,4,4,5,6,6,7,8,8,8,9,9,9,10)
    println(b)
    println(b.rle)

    println(s"sumo of all elements in $aSmallList is $aSmallList.reduce(_ + _)")
    val a = RList.from(1 to 10)
    println(a.duplicateEach(2))

    for {
      i <- 0 to 20
    } println(a.rotate(i))

    println(aLargeList.sample(14))

    // flatMap and filter on a big ass RList
    var currentTime = System.currentTimeMillis()
    aLargeList.flatMap(x => RList(x, x + 1))
    var nowTime = System.currentTimeMillis()
    println(s"flatMap takes ${nowTime - currentTime} ms on 10 000 elements")

    currentTime = System.currentTimeMillis()
    println(aLargeList.reduce(_ + _))
    nowTime = System.currentTimeMillis()
    println(s"reduce takes ${nowTime - currentTime} ms on 10 000 elements")
  }

  def testHardProblems(): Unit = {

    val ordering = Ordering.fromLessThan[Int](_ < _)
    var baseList = aLargeList.sample(30)
    println(s"to be sorted: $baseList")
    println(s"insertion sort result is ${baseList.insertionSort(ordering)}")
    println(s"merge sort result is ${baseList.mergeSort(ordering)}")
    println(s"quick sort result is ${baseList.quickSort(ordering)}")

    baseList = aLargeList.sample(10000)
    var currentTime = System.currentTimeMillis()
    baseList.insertionSort(ordering)
    var nowTime = System.currentTimeMillis()
    println(s"insertion sort takes ${nowTime - currentTime} ms on 10000 elements")

    currentTime = System.currentTimeMillis()
    baseList.mergeSort(ordering)
    nowTime = System.currentTimeMillis()
    println(s"merge sort takes ${nowTime - currentTime} ms on 10000 elements")

    currentTime = System.currentTimeMillis()
    baseList.quickSort(ordering)
    nowTime = System.currentTimeMillis()
    println(s"quick sort takes ${nowTime - currentTime} ms on 10000 elements")
  }

  testHardProblems()
}

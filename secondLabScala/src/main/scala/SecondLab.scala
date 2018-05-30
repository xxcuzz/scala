object SecondLab {
  def main(args: Array[String]):Unit= {
    //Task 1    (VARIANT 13)
    val list1 = List[Int](4, 2, 88, -100, -1, 0, 87)
    val answer1 = (-100, 88)
    assert(SecondLab.minmaxTuple(list1) == answer1)

    //Task 2    (VARIANT 7)
    val list2 = List[Int](1, 3, 1, -2, 4)
    val answer2 = -24
    assert(SecondLab.product(list2) == answer2)

    //Task 3    (VARIANT 12)
    val list3 = List[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 27, 55)
    val answer3 = List[Int](1, 2, 3, 5, 8, 13, 55)
    assert(SecondLab.deleteNonFib(list3) == answer3)
  }

  def deleteNonFib(list3: List[Int]): List[Int] = {
    equalsLists(onlyFibonacciNumbers(list3.max), list3)
  }
  def onlyFibonacciNumbers(cnt: Int, low: BigInt = 0, high: BigInt = 1, sofar: List[BigInt] = List()): List[BigInt] = {
    if (cnt == 0)
      (low :: sofar).reverse
    else
      onlyFibonacciNumbers(cnt - 1, high, low + high, low :: sofar)
  }
  def equalsLists(fib: List[BigInt], listThree: List[Int]): List[Int] = {
    listThree.filter(fib.contains(_))
  }

  def minmaxTuple(simpleList: List[Int]):(Int, Int)={
    if(simpleList.nonEmpty){
      (simpleList.min, simpleList.max)
    }
    else {
      (0,0)
    }
  }

  def product(listInt: List[Int]): Int = {
    if (listInt.nonEmpty) {
      //listInt.reduceLeft(_*_)     //без начального значения
      listInt.foldLeft(1)(_*_)
    }
    else {
      0
    }
  }
}
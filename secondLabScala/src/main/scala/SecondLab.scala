object SecondLab {
  def main(args:Array[String]):Unit= {
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
    assert(SecondLab.onlyFibonacciNumbers(list3) == answer3)
  }

  def onlyFibonacciNumbers(thirdList: List[Int]): List[Int]={

  }

  def minmaxTuple(simpleList: List[Int]):(Int, Int)={
    if(simpleList.nonEmpty){
      (simpleList.sorted.head, simpleList.sorted.last)

      //println(integerTuple._1)      //отладка принтами
      //println(integerTuple._2)
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
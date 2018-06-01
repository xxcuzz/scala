import scala.annotation.tailrec

object FirstLab {
  def main(args: Array[String]):Unit= {

    //Task 1    (VARIANT 7)
    val listOne = List[Int](-4, 0, 1, 5, 18, -1)
    val answerOne = List[Int](-2, 0, 2, 6, 9, 0)

    //assert(FirstLab.taskOne(listOne) == answerOne)
    assert(FirstLab.taskOneTailRec(listOne) == answerOne)
    assert(FirstLab.taskOneRec(listOne) == answerOne)

    //print(FirstLab.taskOneRec(listOne).toString())    //debug
    //print(FirstLab.taskOneTailRec(listOne).toString())//debug

    // Task 2    (VARIANT 3)
    val myStr:String = "nervous rhythm"
    val answerTwo:String = "mhtyhr suovren"

    assert(FirstLab.stringTailRecReverse(myStr) == answerTwo)
    assert(FirstLab.stringRecReverse(myStr) == answerTwo)

    //print(FirstLab.stringTailRecReverse(myStr))       //debug
    // print(FirstLab.stringRecReverse(myStr))          //debug
  }

  def taskOne(firstList: List[Int]): List[Int] = {
    firstList.map(c => if (c % 2 == 0) c/2 else c + 1)
  }

  //@tailrec      //finally non-tail
  def taskOneRec(firstList: List[Int]): List[Int] = {
    if(firstList.length != 0) {
      if (firstList(0) %2 == 0) {
        val a = firstList.updated(0,firstList(0) / 2).head
        a::taskOneRec(firstList.slice(1,firstList.length))
      } else {
        val a = firstList.updated(0,firstList(0) + 1).head
        a::taskOneRec(firstList.slice(1,firstList.length))
      }
    } else {
      firstList
    }
  }

  def taskOneTailRec(firstList: List[Int]): List[Int] = {
    @tailrec
    def task(firstList: List[Int], ind: Int): List[Int] = {
      if (ind == firstList.length) {
        firstList
      } else if (firstList(ind) % 2 == 0) {
        task(firstList.updated(ind, firstList(ind) / 2),ind +1)
      } else {
        task(firstList.updated(ind, firstList(ind) + 1),ind +1)
      }
    }
    task(firstList,0)
  }

  def stringTailRecReverse(str:String):String= {
    @tailrec    // You annotate methods that must be optimized, so that you will get a compile error, when they can't be optimized.
    def stringTailRec(helpStr: String, ind: Int): String = {
      if (ind >= 0)
        stringTailRec(helpStr.concat(str.charAt(ind).toString), ind - 1)
      else
        helpStr
    }
    stringTailRec("", str.length-1)
  }

  //@tailrec   //this is not tail recursion
  def stringRecReverse(str: String): String = {
    if(str.length == 0)
      str
    else
      stringRecReverse(str.substring(1)).concat(str.charAt(0).toString)
  }
}

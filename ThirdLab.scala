object ThirdLab{
  def main(args: Array[String]):Unit= {
    //Task 1    (VARIANT 3)
    //case _ => 0   //MatchError
    val firstStr = "_Oh_Hi Mark "
    val answerOne = " Oh Hi_Mark_"

    assert(swap(firstStr) == answerOne)
    //print(swap(firstStr))         //debug


    //Task 2    (VARIANT 10)

    val human = new Human("Ivan","Pankov","Ivanovich","qqq")
    val answerTwo = "Ivan qqq"

    assert(secondFunction(human) == answerTwo)
    //print(secondFunction(human))

    val canine = new Canine("Bow-wow!","West Highland White Terrier")
    val answerThree = "Bow-wow! West Highland White Terrier"

    assert(secondFunction(canine) == answerThree)
    //print(secondFunction(canine))
  }

  def secondFunction(animal: Animal): String = {
    animal match {
      case Human(_, _, _, _) => animal.onlyNameAndRace()
      case Canine(_, _) => animal.unapply()
    }
  }

  abstract class Animal {
    def unapply(): String{}
    def onlyNameAndRace(): String{}
  }

  case class Human(val name: String, val surname: String, val patronymic: String, val race: String) extends Animal {
    def unapply(): String = {
      name + " " + surname +  " " + patronymic +  " " + race
    }

    def onlyNameAndRace(): String = {
      name + " " + race
    }
  }
  case class Canine(val dogname: String, val breed: String) extends Animal {
    def unapply(): String = {
      dogname + " " + breed
    }

    def onlyNameAndRace(): Unit = {}
  }

  def swap (string: String):String = {
    string.map { case ' ' => '_'  case '_' => ' '  case c => c }
  }
}

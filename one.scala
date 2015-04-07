// Problems
// else branch gets false positives

object Color extends Enumeration {
  type Color = Value
  val Red, Blue, Green = Value
}


object Main extends App {

  import Color._
  import IO._

  case class Person(name: String, age: Int, color: Color)

  print("enter a name: ")
  val name = getLine
  print("enter an age: ")
  val age = toAge(getLine)
  print("enter a color: ")
  val color = toColor(getLine)

  val person = Person(name, age, color)
  printPerson(person)



  def printPerson(p: Person) = {
    println(name + " is " + p.age + " years old; " +  p.name + "'s favorite color is " + p.color)
  }

  def toAge(s: String) : Int = {
    Integer.parseInt(s)
  }

  def toColor(s : String) : Color = {
    if( s == "red" ) {
      Red
    } else if (s == "green" ){
      Green
    } else  {
      Blue
    }
  }


}

object IO {
  val scan = new java.util.Scanner( System.in )
  def getLine() =  {
    scan.nextLine
  }
}


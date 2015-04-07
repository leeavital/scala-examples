// We've changed all of the unsafe functions to return an Option and made sure both were defined
// problem:
// deep nesting of match-case
// errors come from the caller, not the callee

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

  // this won't compile because age, and color are Option[Int] and
  // Option[Color] respectively
  val person =  Person(name, age, color)

  color match {
    case Some(color) =>
      age match {
        case Some(age) =>
          printPerson(Person(name, age, color))
        case None =>
          println("error: bad age")
      }
    case None =>
      println("error: bad color")
  }

  def toAge(s: String) : Option[Int] = {
    try {
      Some(Integer.parseInt(s))
    } catch {
      case e: Exception => None
    }
  }

  def toColor(s : String) : Option[Color] = {
    if( s == "red" ) {
      Some(Red)
    } else if (s == "green" ){
      Some(Green)
    } else if( s == "blue") {
      Some(Blue)
    } else {
      None
    }
  }

  def printPerson(p: Person) = {
    println(name + " is " + p.age + " years old; " +  p.name + "'s favorite color is " + p.color)
  }
}

object IO {
  val scan = new java.util.Scanner( System.in )
  def getLine() =  {
    scan.nextLine
  }

}

// problem:
//  exceptions are unmanaged (toAge and toColor are unsafe.) If I type in the age
//  wrong, the program explodes. Notice that scala does not have a notion of managed vs unmanaged
//  exceptions


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

  def toAge(s: String) : Int = {
    Integer.parseInt(s)
  }

  def toColor(s : String) : Color = {
    if( s == "red" ) {
      Red
    } else if (s == "green" ){
      Green
    } else if( s == "blue") {
      Blue
    } else {
      throw new Exception("Bad color: " + s)
    }
  }



  def printPerson(p: Person) = {
    println(name + " is " + p.age + " years old; " +  p.name + "'s favorite color is " + p.color)
  }


}

// look how easily we can use the java standard library!
object IO {
  val scan = new java.util.Scanner( System.in )
  def getLine() =  {
    scan.nextLine
  }
}


object Color extends Enumeration {
  type Color = Value
  val Red, Blue, Green = Value
}


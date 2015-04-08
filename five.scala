// Let's introduce some more functionality imperatively first
//
// Notice that we now use "var" instead of "val"
//
// Think about how many typos could break introduce runtime errors
// 
// what happens if we:
//  forget to increment i
//  error on all inputs
//  forget to check for null


object Main extends App {
  import Color._
  import IO._


  case class Person(name: String, age: Int, color: Color)

  var i = 0
  var best : Person = null  // I have to annotate the type
  while(i < 3) {
    val p = readPerson
    p match {
      case Right(p) =>
        if( best  == null  || best.age < p.age ) {
          best = p
        }
      case Left(e) => println(e)
    }
    i = i + 1
  }
  println("The oldest person is: " + best.name)


  def readPerson : Either[String,Person] = {
    print("enter a name: ")
    val name  = getLine
    print("enter an age: ")
    for {
      age <- toAge(getLine).right
      _ <- Right(print("enter a color: ")).right
      color <- toColor(getLine).right
    } yield Person(name, age, color)
  }


  def toAge(s: String) : Either[String,Int] = {
    try {
      val i = Integer.parseInt(s)
      Right(i)
    } catch {
      case e: Exception => Left("invalid age")
    }
  }

  def toColor(s : String) : Either[String, Color] = {
    if( s == "red" ) {
      Right(Red)
    } else if (s == "green" ){
      Right(Green)
    } else if( s == "blue") {
      Right(Blue)
    } else {
      Left("invalid color")
    }
  }

  def printPerson(p: Person) = {
    println(p.name + " is " + p.age + " years old; " +  p.name + "'s favorite color is " + p.color)
  }
}

object IO {
  val scan = new java.util.Scanner( System.in )
  def getLine =  scan.nextLine

}

object Color extends Enumeration {
  type Color = Value
  val Red, Blue, Green = Value
}

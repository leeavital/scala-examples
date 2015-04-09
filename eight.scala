// Another common iterative control flow pattern
object Main extends App {
  import Color._
  import IO._


  case class Person(name: String, age: Int, color: Color)

  // why do we need: .right.toOption and .flatten?
  val people : Seq[Person] = (1 to 3).toList.map { _ => readPerson.right.toOption }.flatten

  // val age = 0
  // for( p <- person ) {
  //    age += p.age
  // }

  val age1 = people.foldLeft (0) { (n, p) =>
    n + p.age
  }

  val age2 = people.map(_.age).reduce( _ + _ )

  println(s"The calculated ages are: ${age1} ${age2}")

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

// Now we use Either[String,String]
//
// This hits all the bases: typesafe, error handling, returning rich types when there is a possibility for failure
// let's just clean it up a bit

object Color extends Enumeration {
  type Color = Value
  val Red, Blue, Green = Value
}


object Main extends App {
  import Color._
  import IO._


  case class Person(name: String, age: Int, color: Color)

  print("enter a name: ")
  val name  = getLine
  print("enter an age: ")
  val output = for {A
    age <- toAge(getLine).right
    _ <- Right(print("enter a color: ")).right
    color <- toColor(getLine).right
  } yield name + " is " + age + " years old; " +  name + "'s favorite color is " + color

  output match {A
    case Right(o) => println(o)
    case Left(e)=> println(e)
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
}

object IO {
  val scan = new java.util.Scanner( System.in )
  def getLine() =  {
    scan.nextLine
  }
}

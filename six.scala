// Let's rewrite functionally!

object Main extends App {
  import Color._
  import IO._


  case class Person(name: String, age: Int, color: Color)

  // why do we need: .right.toOption and .flatten?
  val people : Seq[Person] = (1 to 3).toList.map { _ => readPerson.right.toOption }.flatten


  def maxBy[A](xs: Seq[A], cmp: A => Int) : Option[A] =  {
    xs match {
      case Nil => None // commenting this out causes a warning
      case x::Nil => Some(x)
      case x::xs =>
        maxBy(xs, cmp).map { m =>
          if ( cmp(x) > cmp(m) ) {
            x
          } else {
            m
          }
        }
    }
  }

  (maxBy[Person](people, _.age)) match {
    case Some(p) => printPerson(p)
    case None => println("no people")
  }



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

// Let's talk about options as lists with 1 or 0 elements.
//
// Now we are using Option.map and Option.flatMap
// There is less nesting, but  we've exacerbated the error problem. We have no idea
// where errors come from!
//
// Notice the lambda/anonymous function sentence, also notice the use an alternative for comprehension


object Color extends Enumeration {
  type Color = Value
  val Red, Blue, Green = Value
}


object Main extends App {
  import Color._
  import IO._


  case class Person(name: String, age: Int, color: Color)


  // print("Enter a name: ")
  // val name = getLine
  // val age = toAge(getLine)
  // val output = age.flatMap { age =>
  //   val color = toColor(getLine)
  //   color.map { color =>
  //     Person(name, age, color)
  //   }
  // }

  // this is an equivalend sugared version
  //
  print("enter a name: ")
  val output = for {
    name <- Some(getLine)
    _ = print("enter an age: ")
    age <- toAge(getLine)
    _ = print("enter a color: ")
    color <- toColor(getLine)
  } yield Person(name, age, color)

  output match {
    case Some(p) => printPerson(p)
    case None => println("error")
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
    println(p.name + " is " + p.age + " years old; " +  p.name + "'s favorite color is " + p.color)
  }
}

object IO {
  val scan = new java.util.Scanner( System.in )
  def getLine() =  {
    scan.nextLine
  }

}

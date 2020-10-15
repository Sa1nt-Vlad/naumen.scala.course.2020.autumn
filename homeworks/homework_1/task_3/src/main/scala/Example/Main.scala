package Example

object Main extends App {
  def printGreeting(greeting: String, fullName: String) =
    println(s"${greeting} Scala! This is ${fullName}!")

  val fullname: String = "Vladislav Savitskiy"
  val greetings: Array[String] = Array("Hello", "Hola", "Guten tag")

  for (greeting <- greetings) {
    printGreeting(greeting, fullname)
    printGreeting(greeting, fullname.reverse)
  }
}


package lecture.section.one

object AdvancedPatternMatching extends App {

  // pattern matching
  val aList = List(1, 2)
  var matchOfList = aList match {
    case Nil => println("List is empty")
    case head :: Nil => println(s"List has only $head")
    case _ => println("List have many members")
  }
  println(matchOfList)

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(arg: Person): Option[(String, Int)] = Some(arg.name, arg.age)

    def unapply(arg: Int): Option[(String)] = Some("Another unapply method")
  }

  val bob = new Person("Bob", 24)
  val matchOfBob = bob match {
    case Person(n, a) => s"Hi $n, you are $a ages years old"
    case _ => ""
  }
  println(matchOfBob)

  // infix pattern
  case class Or[A, B](a: A, b: B)

  val either = Or(2, "two")
  val matchOfEither = either match {
    case a Or b => s"$a -> $b"
  }
  println(matchOfEither)

  // decomposing sequence
  matchOfList = aList match {
    case List(1, _*) => "This is start with 1 and can be contain more member"
  }

  // custom return type for unapply method
  abstract class Wrapper[T] {
    def isEmpty: Boolean

    def get: T
  }

  object PersonWrapper {
    def unapply(arg: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = arg.name.isEmpty

      override def get: String = arg.name
    }
  }

}

package lecture.section.two

object PartialFunction extends App {

  // Int => Int
  // val aFunction = (x: Int) => x - 1
  val aFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  println(aFunction(2))
  // println(aFunction(3)) MatchError

  // List(1, 2, 5) => Int other case is MatchError
  // PF can have only one parameter type
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  println(aPartialFunction(2))
  // println(aPartialFunction(3)) MatchError

  println(aPartialFunction.isDefinedAt(2))
  println(aPartialFunction.isDefinedAt(3))

  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2))
  println(lifted(3))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 3 => 152
  }
  println(pfChain(2))
  println(pfChain(3))

  val mappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 56
    case 3 => 152
  }
  println(mappedList)

}

package exercise.section.one

object AdvancedPatternMatching extends App {

  // question
  // rewrite the code
  val value = 8
  var matchOfValue = value match {
    case x if x > -10 && x < 10 => "single digit"
    case x if x % 2 == 0 => "even number"
    case _ => "other"
  }
  println(matchOfValue)

  // answer
  object SingleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  object EvenNumber {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  matchOfValue = value match {
    case SingleDigit() => "single digit"
    case EvenNumber() => "even number"
    case _ => "other"
  }
  println(matchOfValue)

}

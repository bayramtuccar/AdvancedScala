package exercise.section.two

object PartialFunction extends App {

  // PF
  class ACustomPartialFunction extends PartialFunction[Int, Int] {
    override def apply(x: Int): Int = x match {
      case 1 => 42
      case 2 => 56
      case 3 => 152
      case 5 => 999
    }

    override def isDefinedAt(x: Int): Boolean = x == 1 || x == 2 || x == 3 || x == 5
  }

  val asObject = new ACustomPartialFunction
  println(asObject(2))
  // println(asObject(4)) MatchError

  // A simple chatBot
  object Hello {
    def unapply(arg: String): Option[String] =
      if (arg.contains("Hi") || arg.contains("Hello")) Some("Hi, I am a chatbot")
      else None
  }

  object GoodBy {
    def unapply(arg: String): Option[String] =
      if (arg.contains("Good by") || arg.contains("By by")) Some("See you soon")
      else None
  }

  val chatBot = (arg: String) => arg match {
    case Hello(t) => t
    case GoodBy(t) => t
    case _ => "Next time, I can understand you. I am learning..."
  }

  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)

}

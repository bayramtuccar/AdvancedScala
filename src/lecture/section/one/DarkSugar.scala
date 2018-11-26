package lecture.section.one

object DarkSugar extends App {

  // method with single param
  def sayHelloToDuck(count: Int): String = s"Hello to $count ducks..."

  val description = sayHelloToDuck {
    // can use {}, complex code and return value
    val a = 5
    val b = 10
    val duckCount = a + b
    duckCount
  }
  println(description)
  println(List(1, 2, 3).map { x: Int => x * 2 })

  // single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anAction = new Action {
    override def act(x: Int): Int = x * 3
  }
  println(anAction.act(5))

  // runnable
  val aRunnable = new Thread(() => println("This is run method"))
  aRunnable.start()

  // :: and #::(for stream) method
  // this is right sides operator due to last : char
  println(2 :: 3 :: 4 :: Nil == List(3, 4).::(2))

  // multi word method
  class Teen(name: String) {
    def `and then said`(text: String) = println(s"$name said $text")
  }

  val lilly = new Teen("Lilly")
  lilly `and then said` "\"This is multi word method in Scala\""

  // update method
  val anArray = Array(1, 2, 3)
  anArray(2) = 7 // anArray.update(2, 7)
  println(anArray(2))

  // set and get for private field
  class AClassWithPrivateField {
    private var field: Int = 0

    def member: Int = field

    def member_(value: Int) =
      field = value
  }

  val anObject = new AClassWithPrivateField
  anObject.member_(5)
  println(anObject.member)

}

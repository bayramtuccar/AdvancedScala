package lecture.section.one

import scala.annotation.tailrec

object Recap extends App {
  // variable(
  val aCondition: Boolean = false

  // a code block with useless code
  val aCodeBlock = {
    if (aCondition) 54 // useless code
    56 // always return this
  }

  // Unit = void from other lang
  println("This return Unit type") // return Unit

  // function
  def aFunction(x: Int): Int = x + 1

  // recursion
  @tailrec def factorial(n: Int, accumulator: Int): Int =
    if (n <= 0) accumulator
    else factorial(n - 1, accumulator)

  // OOP
  class Animal

  class Dog extends Animal

  val aDog: Animal = new Dog // polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("Crocodile eating")
  }

  // infix notation
  val aCroc = new Crocodile
  aCroc eat aDog

  // anonymous class
  val aCornivore = new Carnivore {
    override def eat(a: Animal): Unit = println("Carnivore eating")
  }
  aCornivore eat aDog

  // generic
  abstract class MyList[+A]

  object MyList // singleton and companion

  // case class
  case class Person(name: String, age: Int)

  // exception
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => e.getStackTrace
  } finally {
    println("Do final job like resource release")
  }

  // FP
  val incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val anonymousIncrementer = (x: Int) => x + 1

  // collection
  // List, Set, Seq, Vector, Map, Tuple vs
  // map, flatMap, filter
  val pair = for {
    num <- List(1, 2, 3) if (num % 2 != 0)
    char <- List('a', 'b', 'c')
  } yield num + "-" + char

  val aMap = Map(
    "one" -> 1, "two" -> 2
  )

  // option
  val anOption = Some(2)
  val anotherOption = None

  // pattern matching
  val foo = Person("foo", 30)
  foo match {
    case Person(_, _) => println("Do some action for all Person object")
  }

}
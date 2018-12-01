package exercise.section.two

import scala.annotation.tailrec

trait TSet[A] extends (A => Boolean) {
  def apply(elem: A): Boolean = this contain elem

  def contain(elem: A): Boolean

  def +(elem: A): TSet[A]

  def ++(that: TSet[A]): TSet[A]

  def map[B](func: A => B): TSet[B]

  def flatMap[B](func: A => TSet[B]): TSet[B]

  def filter(func: A => Boolean): TSet[A]

  def foreach(func: A => Unit): Unit

  def -(elem: A): TSet[A]

  def --(that: TSet[A]): TSet[A]

  def &(that: TSet[A]): TSet[A]

  def unary_! : TSet[A]
}

object TSet {
  def apply[A](values: A*): TSet[A] = {
    @tailrec def build(value: Seq[A], acc: TSet[A]): TSet[A] = {
      if (value.isEmpty) acc
      else build(value.tail, acc + value.head)
    }

    build(values.toSeq, new EmptySet[A])
  }
}

class EmptySet[A] extends TSet[A] {
  def contain(elem: A): Boolean = false

  def +(elem: A): TSet[A] = new FiniteSet[A](elem, this)

  def ++(that: TSet[A]): TSet[A] = that

  def map[B](func: A => B): TSet[B] = new EmptySet[B]

  def flatMap[B](func: A => TSet[B]): TSet[B] = new EmptySet[B]

  def filter(func: A => Boolean): TSet[A] = this

  def foreach(func: A => Unit): Unit = {}

  def -(elem: A): TSet[A] = this

  def --(that: TSet[A]): TSet[A] = this

  def &(that: TSet[A]): TSet[A] = this

  def unary_! : TSet[A] = new InfiniteSet[A]
}

class FiniteSet[A](head: A, tail: TSet[A]) extends TSet[A] {
  def contain(elem: A): Boolean = (elem equals head) || (tail contain elem)

  def +(elem: A): TSet[A] = if (this contain elem) this else new FiniteSet[A](elem, this)

  def ++(that: TSet[A]): TSet[A] = tail ++ that + head

  def map[B](func: A => B): TSet[B] = (tail map func) + func(head)

  def flatMap[B](func: A => TSet[B]): TSet[B] = (tail flatMap func) ++ func(head)

  def filter(func: A => Boolean): TSet[A] = {
    if (func(head)) (tail filter func) + head
    else tail filter func
  }

  def foreach(func: A => Unit): Unit = {
    func(head)
    tail foreach func
  }

  def -(elem: A): TSet[A] = {
    if (head equals elem) tail
    else tail - elem + head
  }

  def --(that: TSet[A]): TSet[A] = filter(!that)

  def &(that: TSet[A]): TSet[A] = filter(that)

  def unary_! : TSet[A] = new FunctionSet[A](x => !this.contain(x))
}

class FunctionSet[A](func: A => Boolean) extends TSet[A] {
  def contain(elem: A): Boolean = func(elem)

  def +(elem: A): TSet[A] = new FunctionSet[A](any => func(any) || (any equals elem))

  def ++(that: TSet[A]): TSet[A] = new FunctionSet[A](any => func(any) || that(any))

  def map[B](mapFunc: A => B): TSet[B] = throw new StackOverflowError()

  def flatMap[B](flatMapsFunc: A => TSet[B]): TSet[B] = throw new StackOverflowError()

  def filter(filterFunc: A => Boolean): TSet[A] = new FunctionSet[A](x => func(x) && filterFunc(x))

  def foreach(forEachFunc: A => Unit): Unit = throw new StackOverflowError()

  def -(elem: A): TSet[A] = filter(any => !any.equals(elem))

  def --(that: TSet[A]): TSet[A] = filter(!that)

  def &(that: TSet[A]): TSet[A] = filter(that)

  def unary_! : TSet[A] = new FunctionSet[A](any => !func(any))
}

class InfiniteSet[A] extends TSet[A] {
  def contain(elem: A): Boolean = true

  def +(elem: A): TSet[A] = this

  def ++(that: TSet[A]): TSet[A] = this

  def map[B](func: A => B): TSet[B] = throw new StackOverflowError()

  def flatMap[B](func: A => TSet[B]): TSet[B] = throw new StackOverflowError()

  def filter(func: A => Boolean): TSet[A] = new FunctionSet[A](_ => true) filter func

  def foreach(func: A => Unit): Unit = throw new StackOverflowError()

  def -(elem: A): TSet[A] = new FunctionSet[A](_ => true) - elem

  def --(that: TSet[A]): TSet[A] = filter(!that)

  def &(that: TSet[A]): TSet[A] = filter(that)

  def unary_! : TSet[A] = new EmptySet[A]
}

object TSetPlayground extends App {
  // Empty set
  val aEmptySet = new EmptySet[Int]
  println("aEmptySet(1)= " + aEmptySet(1))
  println("aEmptySet + 1 contain 1= " + (aEmptySet + 1 contain 1))
  println("aEmptySet - 1 contain 1= " + (aEmptySet - 1 contain 1))
  println()

  // Finite set
  val aFiniteSet = TSet(1, 2, 3)
  // aFiniteSet foreach println
  println("aFiniteSet(3)= " + aFiniteSet(3))
  println("aFiniteSet(4)= " + aFiniteSet(4))
  println("aFiniteSet + 4 contain 4= " + (aFiniteSet + 4 contain 4))
  println("aFiniteSet ++ TSet(4) contain 4= " + (aFiniteSet ++ TSet(4) contain 4))
  println("aFiniteSet - 3 contain 3= " + (aFiniteSet - 3 contain 3))
  println("aFiniteSet -- TSet(3) contain 3= " + (aFiniteSet -- TSet(3) contain 3))
  val aStringFiniteSet = aFiniteSet map (x => x match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case _ => "?"
  })
  aStringFiniteSet foreach (elem => print(elem + ","))
  println()

  def even(value: Int): Boolean = value % 2 == 0

  def odd(value: Int): Boolean = !even(value)

  var aFilteredFiniteSet = aFiniteSet filter even
  aFilteredFiniteSet foreach (elem => print(elem + ","))
  println()
  aFilteredFiniteSet = aFiniteSet filter odd
  aFilteredFiniteSet foreach (elem => print(elem + ","))
  println()
  println()

  // function set
  val aFunctionSet = !aFiniteSet
  // aFunctionSet foreach println // StackOverflowError
  println("aFunctionSet(3)= " + aFunctionSet(3))
  println("aFunctionSet + 3 contain 3= " + (aFunctionSet + 3 contain 3))
  println("aFunctionSet ++ TSet(3) contain 3= " + (aFunctionSet ++ TSet(3) contain 3))
  println("aFunctionSet - 4 contain 4= " + (aFunctionSet - 4 contain 4))
  println("aFunctionSet -- TSet(4) contain 4= " + (aFunctionSet -- TSet(4) contain 4))
  println("aFunctionSet(4)= " + aFunctionSet(4))

  /*val aStringFunctionSet = aFunctionSet map (x => x match { // StackOverflowError
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case _ => "?"
  })*/
  var aFilteredFunctionSet = aFunctionSet filter even
  println("(even)aFilteredFunctionSet(4)= " + aFilteredFunctionSet(4))
  aFilteredFunctionSet = aFunctionSet filter odd
  println("(odd)aFilteredFunctionSet(4)= " + aFilteredFunctionSet(4))
  println()

  // Infinite set
  val aInfiniteSet = new InfiniteSet[Int]
  println("aInfiniteSet(1)= " + aInfiniteSet(1))
  println("aInfiniteSet + 1 contain 1= " + (aInfiniteSet + 1 contain 1))
  println("aInfiniteSet - 1 contain 1= " + (aInfiniteSet - 1 contain 1))

}

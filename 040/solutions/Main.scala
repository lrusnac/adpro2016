// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing
object Tests extends App {

  val l1: Stream[Int] = Empty
  val l2: Stream[Int] = empty

  val l3: Stream[Int] = cons(1, cons(2, cons(3, empty)))

  println(l1.headOption)
  println(l2.headOption)
  println(l3.headOption)

  val naturals: Stream[Int] = Stream.from(0)

  println(l3.toList)

  println(Stream.to(10).toList)

  println(naturals.take(1000000000).drop(41).take(10).toList)

  println(naturals.takeWhile(_<1000000000).drop(100).take(50).toList)

  println(naturals.forAll (_ < 0))

  // println(naturals.forAll (_ >=0))

  println(naturals.takeWhileWF(_<1000000000).drop(100).take(50).toList)
  println(naturals.takeWhileWF(_<1000000000).drop(100).take(50).headOptionWF())

  println(naturals.map(_*2).drop(30).take(50).toList)
  println(naturals.drop(42).filter(_%2 ==0).take(30).toList)
  naturals.append(naturals)
  println(naturals.take(10).append(naturals).take(20).toList)

  //println(naturals.flatMap(a => Stream.to(a)).take(100).toList)
  //println(naturals.flatMap (x =>from (x)).take (100).toList)

  val fibs = Stream.fibonaci.take(10).toList
  println(fibs)

  println(empty.fibunfold.take(10).toList)
  println(naturals.mapunfold(_*2).drop(30).take(50).toList)
}
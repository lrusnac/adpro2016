// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness.Stream._
import fpinscala.laziness._

// this is how we do simple interactive testing

object Main extends App {

  val l1: Stream[Int] = Empty
  val l2: Stream[Int] = empty

  val l3: Stream[Int] = cons(1, cons(2, cons(3, empty)))

  println(l1.headOption)
  println(l2.headOption)
  println(l3.headOption)


  val naturals: Stream[Int] = Stream.from(0)

  println(Stream.to(10).toList)

  println(naturals.take(1000000000).drop(41).take(10).toList)

  println(naturals.takeWhile(_<1000000000).drop(100).take(50).toList)

  println(naturals.forAll (_ < 0))

  //keeps evaluating, never to return a value
  //println(naturals.forAll (_ < 0))

  println(naturals.takeWhile1(_<1000000000).drop(100).take(50).toList)

  println(naturals.append(naturals))
  println(naturals.take(10).append(naturals).take(20).toList)


  println(naturals.map(x => Stream.to(x).toList).take(3).toList)
  println("flatmap")
  println(naturals.flatMap(to _).take(100).toList)
  println("what we want flatmap to append")
  println(scala.Stream.from(0).flatMap((x) => scala.Stream.from(0).take(x)).take(100).toList)

  println("unfold")
  println(Stream.unfold(0)(s => Some(s,s+1)).take(10).toList)
  println("naturals")
  println(naturals.take(10).toList)

  println(fibRecursive.take(10).toList)

  println(fibAppend.take(10).toList)

  println(naturals.mapUnfold(x=>x).take(100).toList)

  //println(naturals.take1(10).toList)
  println(naturals.takeUnfold(100).toList)

  println(naturals.takeWhile1(_<1000000000).drop(100).take(50).toList)

  println(naturals.zipWith[Int,Int](_+_)(naturals).take(100).toList)
  //println(naturals.zipAl

  println(naturals.zipWith[Int,Int] (_+_) (naturals).take(2000000000).take(20).toList)

  println(naturals.take(100000000).startsWith(naturals.take(10)))

  println(naturals.startsWith(fibUnfold) == false);


  println(Stream(1,2,3).tails.map(x => x.toList).toList)
  println(scala.Stream(1,2,3).tails)
}


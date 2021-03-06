// Advanced Programming

package fpinscala.laziness
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import scala.language.higherKinds

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
//import stream02._ // uncomment to test another version that breaks headOption
// import stream03._ // evaluate tail on headOption

//import stream04._ // evaluates head and tail inside take
//import stream100._ // toList is bad

class StreamSpecJenjLeru extends FlatSpec with Checkers {

  import Stream._

  behavior of "headOption"
//  - it should return None on an empty stream;  (already included in the examples)
//  - it should return some for a non-empty stream;  (already included in the examples)
//  - headOption should not force the tail of the stream.

  // a scenario test:
  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
      yield list2stream (la)

  //will create a stream of integers of infinite size
  def infStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] = {
    val arb = arbitrary[A]
    def inf: Stream[A] = Stream.cons[A](arb.sample.ensuring(_.isDefined).get,inf)
    Gen.const(inf)
  }

  def infStreamExceptions[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] = {
    def inf: Stream[A] = Stream.cons[A](throw new RuntimeException("inf stream of exception forced"),inf)
    Gen.const(inf)
  }

  def prefixedExceptionStream[A](implicit arbA :Arbitrary[A]) :Gen[(Int,Stream[A])] = {
    val arb = arbitrary[A]

    def toN(n:Int): Stream[A] = {
      if (n>0) Stream.cons(throw new RuntimeException("exception stream forced"),toN(n-1))
      else Stream.cons[A](arb.sample.ensuring(_.isDefined).get,toN(-100))
    }

    for {
      length <- Gen.choose(0,100)
      s <- Gen.const(toN(length))

    } yield (length, s)
  }


  // a property test:
  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons(n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )

  }

  it should "not force the tail of the stream" in {
    cons(100, Stream(throw new RuntimeException("it evaluates the tail"))).headOption
  }

  behavior of "take"
  // - take should not force any heads nor any tails of the Stream it manipulates
  it should "not force any heads nor any tails of the Stream it manipulates" in {
    cons(throw new RuntimeException("it evaluates the head"),
      Stream(throw new RuntimeException("it evaluates the tail"))).take(1)
    cons(throw new RuntimeException("it evaluates the head"),
      throw new RuntimeException("it evaluates the tail which wasn't even a stream, gasp!")).take(2)
  }

  //  - take(n) does not force (n+1)st head ever (even if we force all elements of take(n))
  it should "not force (n+1)st head ever (even if we force all elements of take(n))" in check {
    implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(0, 1000))
    Prop.forAll{(n :Int) => {
        val streamExceptions = ones.map(x => throw new RuntimeException("forced the n+1"))
        val streamx = ones.take(n).append(streamExceptions)
        streamx.take(n).toList == ones.take(n).toList // in this way we also test the append
      }
    }
  }

  //  - s.take(n).take(n) == s.take(n) for any Stream s and any n
  it should "match all prefix of original stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(0, 100))
    implicit def arbCharInfStream = Arbitrary[Stream[Char]] (infStream[Char])
    ("fixed n" |:
      Prop.forAll{(s :Stream[Int]) => s.take(1).take(1).toList == s.take(1).toList}) &&
    ("generated n" |:
      Prop.forAll{(s :Stream[Int], n: Int) => s.take(n).take(n).toList == s.take(n).toList}) &&
    ("generated n, inf streams" |:
      Prop.forAll{(s :Stream[Char], n: Int) =>
        s.take(n).take(n).toList == s.take(n).toList && s.take(n).toList != s.take(n+1).toList})
  }

  behavior of "drop"
  //  - s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)
  it should "additivity" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(0, 100))
    implicit def arbCharStreamInf = Arbitrary[Stream[Char]](infStream[Char])
    ("fixed size streams" |:
      Prop.forAll{(s :Stream[Int], n:Int, m:Int) => s.drop(n).drop(m).toList == s.drop(n+m).toList})
    }
  //  - s.drop(n) does not force any of the dropped elements heads
  //  - the above should hold even if we force some stuff in the tail
  it should "not force any of the dropped elements heads" in check {
    implicit def prefixedInfStream = Arbitrary[(Int,Stream[Char])](prefixedExceptionStream[Char])
    implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(0, 100))
    ("inf stream with prefixed exceptions" |: Prop.forAll { (x: (Int,Stream[Char])) => {
      x._2.drop(x._1).take(10).toList; //force some amount in the tail
      true
    }
    })
  }


  behavior of "map"
  //  - x.map(id) == x (where id is the identity function)
  it should "identity" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s :Stream[Int]) => s.map(m => m).toList == s.toList}
  }
  //  - map terminates on infinite streams
  it should "terminate on infinite stream" in {
    ones.map(x => x)
  }

  it should "not force anything in second stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s :Stream[Int]) => s.map(x => throw new RuntimeException("map forced")); true }

  }

  behavior of "append"
  //  - propose properties yourself

  // appending nothing to stream is the stream
  it should "nothing append stream = stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s :Stream[Int]) => empty.append(s).toList == s.toList}
  }

  // appending stream to nothing is the stream
  it should "stream append nothing = stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s :Stream[Int]) => s.append(empty).toList == s.toList}
  }

  // appending infinite streams terminates // scenario test
  it should "terminate" in {
    ones.append(ones)
    fibs.append(fibs)
    fibsViaUnfold.append(fibsViaUnfold)
  }

  // appending stream of n elements with stream of m elements has n+m elements // order? => scenario test
  it should "sum of elements" in check {
    implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(0, 1000))
    Prop.forAll{(n:Int, m:Int) => ones.take(n).append(ones.take(m)).toList.size == n+m}
  }
  // appending two streams does not force the tail
  it should "not force the tail" in {
    val stream1 = Stream(1)
    val stream2 = ones.map(x => throw new RuntimeException("forced the stream 2"))
    stream1.append(stream2)
    true
  }


  //it seems appending two streams WILL force the head of stream1 because of foldRight
  it should "not force the tail of stream 1 and not force stream2" in {
    val stream1 = Stream.cons(1,
      ones.map(x => throw new RuntimeException("forced some part of stream1 (not head)")))
    val stream2 = ones.map(x => throw new RuntimeException("forced the stream 2"))
    stream1.append(stream2)
    true
  }

  it should "uphold associativity a(b)(c) == a(b(c))" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(a: Stream[Int], b: Stream[Int], c: Stream[Int]) => a.append(b).append(c).toList == a.append(b.append(c)).toList}
  }

  behavior of "toList"

  it should "return empty list work for empty streams" in {
    assert(Stream().toList == Empty.toList)
    assert(Stream().toList.size == 0)
  }

  it should "should be same size as List.fill(n,0).size" in check {
    implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(0, 1000))
    Prop.forAll{(n:Int) => ones.take(n).toList.size == List.fill(n)(0).size}
  }


}

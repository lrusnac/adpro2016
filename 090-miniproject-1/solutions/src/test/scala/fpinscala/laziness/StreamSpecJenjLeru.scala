// Advanced Programming

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption
// import stream03._ // evaluate tail on headOption

// import stream04._ // evaluates head and tail inside take

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
    cons(Stream(throw new RuntimeException("it evaluates the head")),
      Stream(throw new RuntimeException("it evaluates the tail"))).take(1)
  }

  //  - take(n) does not force (n+1)st head ever (even if we force all elements of take(n))

  //  - s.take(n).take(n) == s.take(n) for any Stream s and any n
  it should "match all prefix of original stream" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(0, 100))
    ("fixed n" |:
      Prop.forAll{(s :Stream[Int]) => s.take(1).take(1).toList == s.take(1).toList}) &&
    ("generated n" |:
      Prop.forAll{(s :Stream[Int], n: Int) => s.take(n).take(n).toList == s.take(n).toList})
  }

  behavior of "drop"
//  - s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)
  it should "additivity" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def arbPositiveInt = Arbitrary[Int] (Gen.choose(0, 100))
    Prop.forAll{(s :Stream[Int], n:Int, m:Int) => s.drop(n).drop(m).toList == s.drop(n+m).toList}
  }
//  - s.drop(n) does not force any of the dropped elements heads
//  - the above should hold even if we force some stuff in the tail

  behavior of "map"
//  - x.map(id) == x (where id is the identity function)
  it should "identity" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll{(s :Stream[Int]) => s.map(m => m).toList == s.toList}
  }
//  - map terminates on infinite streams

  behavior of "append"
//  - propose properties yourself



}

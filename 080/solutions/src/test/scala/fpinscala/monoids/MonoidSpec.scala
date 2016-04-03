// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
// Example solution for scala exercises using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monoids
import org.scalacheck.Prop._
import org.scalacheck._

object MonoidSpec extends Properties("Monoids..") {

  import Monoid._

  def associative[A :Arbitrary] (m: Monoid[A]) :Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      m.op(m.op(a1,a2), a3) == m.op(a1,m.op(a2,a3)) } :| "associativity"

  def unit[A :Arbitrary] (m :Monoid[A]) =
    forAll { (a :A) => m.op(a, m.zero) == a } :| "right unit" &&
    forAll { (a :A) => m.op(m.zero, a) == a } :| "left unit"

  def monoid[A :Arbitrary] (m :Monoid[A]) :Prop = associative (m) && unit (m)

  property ("stringMonoid is a monoid") = monoid (stringMonoid)


  // Exercise 4: test listMonoid, intAddition, intMultiplication, booleanOr,
  // booleanAnd and optionMonoid.
  property ("IntAddition is a monoid") = monoid(intAddition)

  property ("optionMonoid[Boolean] is a monoid") = monoid[Option[Boolean]](optionMonoid)
  property ("ListMonoid is a monoid") = monoid[List[Int]](listMonoid)
  property ("IntMultiplication is a monoid") = monoid[Int](intMultiplication)
  property ("booleanOr is a monoid") = monoid[Boolean](booleanOr)
  property ("booleanAnd is a monoid") = monoid[Boolean](booleanAnd)

  // Exercise 7

  def homomorphism[A :Arbitrary,B :Arbitrary](ma: Monoid[A]) (f: A => B) (mb: Monoid[B]) : Prop = {
    forAll {
      (a1: A, a2: A) => mb.op(f(a1),f(a2)) == f(ma.op(a1,a2))
    } :| "homomorphism"
  }

  //not right I dont think
  def isomorphism[A :Arbitrary, B :Arbitrary](ma: Monoid[A])(f: A=>B)(g: B=>A)(mb: Monoid[B]) : Prop = {
    homomorphism(ma)(f)(mb) && homomorphism(mb)(g)(ma)
  }

  //extra test
  property ("stirngMonoid toList is homomorphic") = homomorphism[String,List[Char]](stringMonoid)(_.toList)(listMonoid)

  property ("stringMonoid and listMonoid[Char] are isomorphic for functions toList,mkString(\"\")") = isomorphism[String,List[Char]](stringMonoid)(_.toList)(_.mkString(""))(listMonoid)

  // Exercise 8

  property ("booleanOr and booleanAnd are isomorphic") = isomorphism[Boolean,Boolean](booleanAnd)(x => !x)(x => !x)(booleanOr)

  // Exercise 9 (the testing part)

  property ("product monoid of is a monoid") = monoid[(Int,String)](productMonoid(intAddition)(stringMonoid))
  //

  // property ("productMonoid is a monoid") =
}

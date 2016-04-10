// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen

// Example solutions for Monad exercises, using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monads
import fpinscala.monads.Monad._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.language.higherKinds


object  MonadSpec extends Properties("Monad[F[_]] laws..") {

  // Note: The law is fine, but remember that scalacheck has presently a very
  // weak function generator (only generates constant functions)
  def associative[A,F[_]] (m: Monad[F]) (implicit a: Arbitrary[F[A]]): Prop =
    forAll { (x: F[A], f: A => F[A], g: A => F[A]) =>
      m.flatMap[A,A] (m.flatMap[A,A] (x) (f)) (g) ==
      m.flatMap (x) (a => m.flatMap (f(a)) (g))
    }

  def identity[A, F[_]] (m: Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]): Prop =
      forAll { (x: F[A], f: A => F[A]) =>
      m.flatMap[A,A] (x) (m.unit[A] (_)) == x } :| "right unit" &&
    forAll { (y :A, f: A => F[A]) =>
      m.flatMap[A,A] (m.unit[A](y)) (f) == f(y) } :| "left unit"

  def monad[A,F[_]] (m :Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]) :Prop =
    associative[A,F] (m) && identity[A,F] (m)

  // uncomment when you have optionMonad
  property ("of optionMonad") = monad[Int,Option] (optionMonad)

  // Exercise 17

  //Then add test properties for list monad over integers, stream monad of integers, and stream monad
  //over strings (in the MonadSpec.scala file).
  property ("of listMonad") = monad[Int,List] (listMonad)
  property ("of streamMonad") = monad[Int,Stream](streamMonad)
  property ("of streamMonad on Strings") = monad[String,Stream](streamMonad)

  // Exercise 19

  // def kleisliAssociative[A,B,C,D,F[_]] ...
  // def kleisliIdentity[A,B,F[_]] ...
  // def kleisliMonad[A,B,C,D,F[_]] ...

  // property ...
  // property ...
  // property ...
  // property ...
  // property ...
}

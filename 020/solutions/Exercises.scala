// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Leonid Rusnac leru@itu.dk
// AUTHOR2: Jens Tuxen Johannessen jenj@itu.dk
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  // override function application to provide a factory of lists (convenience)
  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
  def tail[A] (as: List[A]) :List[A] = as match {
      case Cons(x, xs) => xs
      case _ => Nil
  }

  // Exercise 3
  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
    case Cons(x, xs) => Cons(newHead, xs)
    case _ => Nil
  }

  // Exercise 4
  def drop[A] (l: List[A], n: Int) : List[A] = n match {
    case n if n > 0 => drop(tail(l), n-1)
    case _ => l
  }

  // Exercise 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) => {
      if(f(x)) {
        dropWhile(xs, f)
      } else {
        l
      }
    }
    case _ => Nil
  }

  // Exercise 6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case _ => Nil
    }
  }

  // Exercise 7 is in the bottom of the file

  // Exercise 8
  def length[A](as: List[A]): Int = {
    def l(as: List[A], v: Int): Int = as match {
      case Cons(x, xs) => l(xs, v + 1)
      case _ => v
    }
    l(as, 0)
  }

  // Exercise 9
  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 10
  def sum (as : List[Int]) : Int = foldLeft(as, 0)(_+_)
  def product (as :List[Int]) : Int = foldLeft(as, 1)(_*_)
  def length1 (as :List[Int]) : Int = foldLeft(as, 0)((x, y) => x + 1)

  // Exercise 11
  def reverse[A] (as :List[A]) :List[A] = foldLeft(as, List[A]())((xs, x) => Cons(x, xs))

  // Exercise 12
  // def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = ...

  // def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = ...

  // Exercise 13
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldLeft(as, List[A]())((x, xs) => append(x, xs))
  }

  // Exercise 14
//  def map[A,B](a: List[A]) (f: A => B): List[B] = a match {
//    case Cons(x, xs) => Cons(f(x), map(xs)(f))
//  }

  def map[A,B](a: List[A]) (f: A => B): List[B] = reverse(foldLeft(a, List[B]())((xs, x) => Cons(f(x), xs)))

  // Exercise 15 (no coding)

  // Exercise 16
  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = {
    reverse(foldLeft(as, List[A]())((xs, x) => {
      if(f(x)) Cons(x, xs)
      else xs
    }))
  }

  // Exercise 17
  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = foldLeft(as, List[B]())((xs, x) => append(xs, f(x)))

  // Exercise 18
  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = flatMap(l)(x=>{
    if(p(x)) List(x)
    else List()
  })

  // Exercise 19
  def add (l: List[Int]) (r: List[Int]): List[Int] = (l, r) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, add(xs)(ys))
    case (_, _) => List[Int]()
  }

  // Exercise 20
  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = (l, r) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(f)(xs, ys))
    case (_, _) => List[C]()
  }

  // Exercise 21
  def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = sup match {
      case Nil => sub == Nil
      case Cons(x, xs) =>
          if(hasSubsequenceFromHere(Cons(x, xs), sub) == true) true
          else hasSubsequence(xs, sub)
  }
  def hasSubsequenceFromHere[A] (sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (Cons(x, xs), Cons(y, ys)) => x == y && hasSubsequenceFromHere(xs, ys)
      case (_, Nil) => true
      case (_, _) => false
  }

  // Exercise 22
  // def pascal (n :Int) : List[Int] = ...

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

}

// Exercise 7

object Exercise7 {

  case class SalaryLine(name: String, amount: Integer)

  // def maximumSalary (salaries: List[SalaryLine]) :Integer = ...

  //val test_case = List(SalaryLine("John",41),SalaryLine("Alice", 42), SalaryLine("Bob",40))

}

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

// The extension of App allows writing statements at class top level (the so
// called default constructor). For App objects they will be executed as if they
// were placed in the main method in Java.


object Exercises extends App {

  // Exercise 3
  // A few tests, uncomment when your implementation is ready.
  assert(power(2.0, 2) == 4.0)
  assert(power(1.0, 42) == 1.0)
  //
  // The above assertions should pass when you call "scala Exercises".
  //
  // The following one should fail. Uncomment to check that assert works as
  // expected:
  //
  //assert (power (1.0, 42) == 2.0)
  // add 2-3 more tests:
  //
  // ...
  assert(power(10000, 0) == 1)
  assert(power(-10, 2) == 100)

  // Exercise 4
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibInner(p: Int, current: Int, n: Int): Int = {
      if (n == 0) current
      else fibInner(current, p + current, n - 1)
    }

    if (n > 1) fibInner(0, 1, n - 2)
    else 0
  }

  // some tests (uncomment, add more):
  assert(fib(6) == 5)
  assert(fib(1) == 0)

  //none of the recursive calls are in the tail position, each time,
  //either the multiplier or the divider is in the tail position
  def power(x: Double, n: Int): Double = {
    n match {
      case 0 => 1
      case n if (n > 0 && n % 2 == 0) => power(x, n / 2) * power(x, n / 2)
      case n if (n > 0 && n % 2 == 1) => x * power(x, n - 1)
      case n if n < 0 => 1 / power(x, -n)
    }
  }

  // Exercise 5
  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
    def this(tag: String, price: Int) = {
      this()
      this.tag = tag
      this.price = price
    }

    var tag: String = ""
    // a tag line in the accounting system
    var price: Int = 0 // the price is in cents
  }

  // computes the total of expenses in cents
  def total(expenses: Array[Expense]): Int = {
    @annotation.tailrec
    def t(expenses: Array[Expense], r: Int): Int = {
      if (expenses.length == 0) {
        r
      } else {
        t(expenses.tail, r + expenses.head.price)
      }
    }
    t(expenses, 0)
  }

  val testcase1 = Array[Expense](
    new Expense("Coffee", 450),
    new Expense("Cake", 350))

  assert(total(testcase1) == 800)

  // uncomment
  // Add one or two more tests
  // ...
  // Exercise 6
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def is(ar: Array[A], r: Boolean): Boolean = {
      if (ar.length < 2) {
        r
      } else {
        is(ar.tail, r && ordered(ar.head, ar.tail.head))
      }
    }
    is(as, true)
  }
  // some tests (uncomment)
  assert(isSorted(Array(1, 2, 3, 4, 5, 6), (a: Int, b: Int) => a <= b))
  assert(!isSorted(Array(6, 2, 3, 4, 5, 6), (a: Int, b: Int) => a <= b))
  assert(!isSorted(Array(1, 2, 3, 4, 5, 1), (a: Int, b: Int) => a <= b))
  // add two tests with another type, for example an Array[String]

  // Exercise 7: a curried version of solution to exercise 3
  def power1(x: Double)(n: Int): Double = {
    n match {
      case 0 => 1
      case n if (n > 0 && n % 2 == 0) => (power1(x)(n / 2)) * (power1(x)(n / 2))
      case n if (n > 0 && n % 2 == 1) => x * power1(x)(n - 1)
      case n if n < 0 => 1 / power1(x)(-n)
    }
  }

  // Exercise 8
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => (b => f(a, b))
  }

  // test if it type checks by currying power automatically:
  val power_curried: Double => Int => Double = {
    curry(power)
  }

  // test if it type checks by currying power automatically:
  assert(power_curried(10)(2) == 100)

  // Exercise 9
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  val power_uncurried: (Double, Int) => Double = uncurry(power_curried)
  assert(power_uncurried(10, 2) == 100)

  // Exercise 10
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def f1(i: Int) = i + 1
  def f2(i: Int) = i * 2
  def composition = compose(f1, f2)
  assert(composition(5) == 11)
}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Exercise 1 (CB 6.1)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextint, nextstate) = rng.nextInt
    if(nextint < 0) (nextint * -1, nextstate)
    else (nextint, nextstate)
  }

  // Exercise 2 (CB 6.2)

  def double(rng: RNG): (Double, RNG) = {
    val (nextint, nextstate) = nonNegativeInt(rng)
    (nextint / (Int.MaxValue.toDouble + 1), nextstate)
  }

  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextint, nextstate) = rng.nextInt
    val (nextdouble, nextstated) = double(nextstate)
    ((nextint, nextdouble), nextstated)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((nextint, nextdouble), nextstate) = intDouble(rng)
    ((nextdouble, nextint), nextstate)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)
  }

   def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case n if n>0 => {
      val (nint, nstate) = rng.nextInt
      val (l, nextstate) = ints(n-1)(nstate)
      (nint::l, nextstate)
    }
    case _ => (List(), rng)
  }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

   def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)

  val _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6 (CB 6.6)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // this is given in the book

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldLeft(unit(List[A]()))((listOfA, f) => map2(f, listOfA)(_::_))

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)((a) => unit(a % n))

  // Exercise 9 (6.9)

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))
  val __double: Rand[Double] = _map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(x => _map(rb)(b=> f(x, b)))

  def _both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = _map2(ra, rb)((_, _))
}

import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 10 (6.10)


  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, state) = run(s)
    (f(a), state)
  })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, state) = run(s)
    f(a).run(state)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 10 (6.10) continued

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = sas.foldLeft(unit[S, List[A]](List()))((listOfA, f) => f.map2(listOfA)(_::_))
  //
  // This is given in the book:

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 11

  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    val (a, s2) = s.run(seed)
    Stream(a).append(state2stream(s)(s2))
  }

  // Exercise 12

  val random_integers = state2stream(random_int)(RNG.Simple(42))

}






sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  // Exercise 13 (CB 6.11)

  // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ...
}

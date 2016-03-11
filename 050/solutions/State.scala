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
    val (a,b) = rng.nextInt
    (Math.abs(a),b)
  }

  // Exercise 2 (CB 6.2)

  def double(rng: RNG): (Double, RNG) = {
    val (a,b) = nonNegativeInt(rng)
    (a.toDouble/Int.MaxValue.toDouble,b)
  }

  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (a,ab) = rng.nextInt
    val (d,db) = double(ab)
    ((a,d),db)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((a,d),b) = intDouble(rng)
    ((d,a),b)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (a,rng1) = double(rng)
    val (b,rng2) = double(rng1)
    val (c,rng3) = double(rng2)
    ((a,b,c),rng3)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i, rngNext) = rng.nextInt
      val (r, rngFinal) = ints(count - 1)(rngNext)
      (i :: r, rngFinal)
    }
    else (List(), rng)
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

  val _double: Rand[Double] = map(nonNegativeInt)(a => a.toDouble/(Int.MaxValue.toDouble+1))

  // Exercise 6 (CB 6.6)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a,rng2) = ra(rng)
      val (b,rng3) = rb(rng2)
      (f(a,b),rng3)
  }

  // this is given in the book

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)

  //rng => fs.foldLeft((List(),rng))(x => x._)
  //def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldLeft(unit(List()))(x => map2)

  // def _ints(count: Int): Rand[List[Int]] = ...

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a,b) = f(rng)
    g(a)(b)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(x => unit(x % n))
  }

  // Exercise 9 (6.9)

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))

  //or with map and you don' need unit
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(x => flatMap(rb)(l => unit(f(x,l))))


}



case class State[S, +A](run: S => (A, S)) {

  // Exercise 10 (6.10)

  def map[B](f: A => B): State[S, B] = State(s =>  {
    val (a,s2) = run(s)
    (f(a), s2)
    })


  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
      val (a,s2) = this.run(s)
      val (b,s3) = sb.run(s2)
      (f(a,b),s3)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a,s2) = run(s)
    f(a).run(s2)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 10 (6.10) continued

  //def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = {

  //}
  //
  // This is given in the book:

  // def modify[S](f: S => S): State[S, Unit] = for {
  //   s <- get // Gets the current state and assigns it to `s`.
  //   _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  // } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 11

  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    val (a,s2) = s.run(seed)
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

  //def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ...
}

// vim:cc=80:foldmethod=indent:foldenable

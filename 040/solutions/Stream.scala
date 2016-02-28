// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness
import Stream._


sealed trait Stream[+A] {
  def toList :List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => List()
  }

  def take (n :Int) : Stream[A] = this match {
    case Cons(h,t) => if (n == 0) Empty else cons(h(), t().take(n-1))
    case _ => Empty
  }

  def drop (n :Int) :Stream[A] = this match {
    case Cons(h, t) => if (n == 0) this else t().drop(n-1)
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) => if (p(h())) cons(h(),t().takeWhile(p)) else Empty
    case _ => Empty
  }


  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => if (p(h())) t().forAll(p) else false
    case Empty => true
    case _ => false
  }


  def takeWhile1(p: A => Boolean): Stream[A] = this.foldRight[Stream[A]](Empty)((a,b) => if (p(a)) cons(a,b) else Empty)


  def headOption1 : Option[A] = this.foldRight[Option[A]](None)((a,b) => if (a != Empty) Some(a) else None)

  //foldright
  def map[B](f: A => B): Stream[B] = this.foldRight[Stream[B]](Empty)((a,b) => cons(f(a),b))
  def filter(f: A=>Boolean) = this.foldRight[Stream[A]](Empty)((a,b) => if (f(a)) cons(a,b) else b)
  def append[B>:A](s: =>Stream[B]): Stream[B] = this.foldRight(s)((a,b) => cons(a,b))
  def flatMap[B](f: A=>Stream[B]) = this.foldRight[Stream[B]](Empty)((x,xs) => {
    //0).foldRight(empty)((a,b) => f(a).append(b)
    //a=0,b=empty
    //f(0).append(())
    //f(0) = (0), a = empty, b = (0)
    //(0).append((0))
    //
    f(x).append(xs)
  })


  //unfold
  //map, take, takeWhile, zipWith, and zipAll
  def mapUnfold[B](f:  A=>B): Stream[B] = unfold(this)(x => x match {
    case Cons(h,t) => Some(  (f(h()), t()  ) //not sure why t(), i guess t is a function => Stream[A]
    case _ => None
  })

  def takeUnfold(n: Int): Stream[A] = unfold((n,this))(x => x match {
    case (i: Int, Cons(h,t)) => {
      if (i >= 0) Some(h(), (i-1, t()))
      else None
    }
    case _ => None
  })

  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold((true,this))(x => x match {
    case (true,Cons(h,t)) => if (p(h())) Some( (h(), (true,t())   ) ) else None
    case _ => None //(false,... or anything else
  })

  //not asked for
  def zip[B](s2: Stream[B]): Stream[(A,B)] = Stream.unfold((this,s2))(x => x match {
    case (Cons(h,t), Cons(h1,t1)) => Some( (h(),h1()), (t(),t1()))
    case _ => None
  })


  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold((this,s2))(x => x match {
    case (Cons(h,t), Cons(h1,t1)) => Some( (Some(h()),Some(h1())), (t(),t1()))
    case (x: Stream[A],Cons(h1,t1)) => Some( (None,Some(h1())), (x,t1()))
    case (Cons(h,t),y: Stream[B]) => Some( (Some(h()),None), (t(),y))
    case _ => None
  })

  def zipWith[B,C](f: (A,B)=>C)(s2: Stream[B]) = Stream.unfold((this,s2))(x => x match {
    case (Cons(h,t),Cons(h1,t1)) => Some ( (f(h(),h1()),(t(),t1())) )
    case _ => None
  })


  def startsWith[A](that: Stream[A]): Boolean =  this.zipAll(that).takeWhile(p => p._1.isDefined && p._2.isDefined).forAll(p => p._1.equals(p._2))

  def tails: Stream[Stream[A]] = Stream.unfold(this)(s => if (s==empty) None else Some(s,s.tail))


  //filter is only evaluated if possible, for head
  def find (p :A => Boolean) :Option[A]= this.filter(p).headOption

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f(h(), t().foldRight(z)(f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f(h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption
}




case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def append[A,B>:A](a: => Stream[A])(s: =>Stream[B]): Stream[B] = a.foldRight(s)((a,b) => cons(a,b))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map((x:(A,S)) => cons(x._1,unfold(x._2)(f))).getOrElse(Empty)
  val fibInf: Stream[Int] = Stream.unfold((0,1))(s => Some((s._2, (s._2,s._1+s._2))))

  //to zero not infinite
  def to (n :Int) :Stream[Int] = {
    def t(c: Int): Stream[Int] = if (c == n) cons(c,Empty) else cons(c,t(c+1))
    t(0)
  }

  def from(n :Int) :Stream[Int] = cons(n, from(n+1))

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }



  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq
}

// vim:tw=0:cc=80:nowrap

package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import scala.collection.View.Empty
import u03.Lab03.Sequence.*

object Lab03:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

      def sum(l: Sequence[Int]): Int = l match
        case Cons(h, t) => h + sum(t)
        case _          => 0

      def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
        case Nil()      => Nil()

      def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
        case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
        case Cons(_, t)            => filter(t)(pred)
        case Nil()                 => Nil()

      // Lab 03
      def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
        case (Cons(h1, t1), Cons(h2,t2)) => Cons((h1,h2), zip(t1,t2))
        case _ => Nil()

      def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0 => Cons(h, take(t)(n-1))
        case _ => Nil()
      
      def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
        case Cons(h1, t1) => Cons(h1, concat(t1, l2))
        case _ => l2

      
      def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case _ => Nil()    

      def min(l: Sequence[Int]): Optional[Int] = l match
        case Cons(h, Cons(h1, t1)) if h > h1 => min(Cons(h1, t1))
        case Cons(h, Cons(h1, t1)) => min(Cons(h,t1))
        case Cons(h, Nil()) => Optional.Just(h)
        case _ => Optional.Empty()

      def foldLeft(s: Sequence[Int])(d: Int)(f: (Int, Int) => Int): Int = s match
        case Cons(h, tail) => foldLeft(tail)(f(d, h))(f)
        case _ => d

    //part 2

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def year(p: Person): Int = p match
      case Student(_, y) => y
      case _ => 0

    def course(p: Person): String = p match
      case Teacher(_, c) => c
      case _ => "None"


    import Person.*
    import Sequence.*
    
    def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

    def coursesList(s: Sequence[Person]): Sequence[String] = map[Person, String](filter[Person](s)(!isStudent(_)))(Person.course(_))

  object SequenceExtension:

    import Person.*
    import Sequence.*

    extension (l: Sequence[Int])
      def sumExt(): Int = l match
        case Cons(h, t) => h + t.sumExt()
        case _          => 0

      def minExt(): Optional[Int] = l match
        case Cons(h, Cons(h1, t1)) if h > h1 => Cons(h1, t1).minExt()
        case Cons(h, Cons(h1, t1)) => Cons(h,t1).minExt()
        case Cons(h, Nil()) => Optional.Just(h)
        case _ => Optional.Empty() 

      def foldLeftExt()(d: Int)(f: (Int, Int) => Int): Int = l match
        case Cons(h, tail) => tail.foldLeftExt()(f(d, h))(f)
        case _ => d

    extension [A](l: Sequence[A])
      def mapExt[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.mapExt(mapper))
        case Nil()      => Nil()

      def filterExt(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filterExt(pred))
        case Cons(_, t)            => t.filterExt(pred)
        case Nil()                 => Nil()

      def zipExt[B](s: Sequence[B]): Sequence[(A, B)] = (l, s) match
        case (Cons(h1, t1), Cons(h2,t2)) => Cons((h1,h2), t1.zipExt(t2))
        case _ => Nil()

      def takeExt(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0 => Cons(h, t.takeExt(n-1))
        case _ => Nil()
      
      def concatExt(s: Sequence[A]): Sequence[A] = l match
        case Cons(h1, t1) => Cons(h1, t1.concatExt(s))
        case _ => s

      
      def flatMapExt[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => mapper(h).concatExt(t.flatMapExt(mapper))
        case _ => Nil()     
    
    extension (l: Sequence[Person])
      def coursesListExt(): Sequence[String] = l.filterExt(!isStudent(_)).mapExt[String](Person.course(_))

  //part 3
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def fill[A](limit: Int)(next: A): Stream[A] = limit match
      case n if n >0 => cons(next, fill(limit-1)(next))
      case _ => empty()
      
    def pellGenerator(v1: Int, v2: Int): Stream[Int] = cons[Int](v2, pellGenerator(v1*2+v2, v1))
    
    val pell: Stream[Int] = pellGenerator(1, 0)
    
  
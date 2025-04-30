package u03

import u02.Modules.{Person, course, isStudent}
import u02.Modules.Person.*
import u03.Optionals.Optional
import u03.Optionals.Optional.*

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(head, tail) if n > 0 => skip[A](tail)(n-1)
      case Cons(head, tail) => Cons(head, tail)
      case _ => Nil()


    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h, t), Cons(h2, t2)) => Cons((h,h2), zip(t,t2))
      case _ => Nil()

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1,s2) match
      case (Cons(h,t), s2) => Cons(h, concat[A](t, s2))
      case (Nil(), s2) => s2

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h,t) => concat[A](reverse[A](t), Cons(h,Nil()))
      case _ => Nil()



    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()


    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] = s match
      case Cons(h, Cons(h2, t)) if h < h2 => min(Cons(h, t))
      case Cons(h, Cons(h2, t)) => min(Cons(h2, t))
      case Cons(h, _) => Optional.Just(h)
      case _ => Optional.Empty()



    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, Cons(h2, t)) => Cons(h, evenIndices[A](t))
      case Cons(h, Nil()) => Cons(h, Nil())
      case _ => Nil()


    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h,t) if h==elem => true
      case Cons(h,t) => contains[A](t)(elem)
      case _ => false



    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) if contains[A](t)(h) => Cons(h, distinct[A](filter(t)(_!=h)))
      case Cons(h,t) => Cons(h, distinct[A](t))
      case _ => Nil()



    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = s match
      case Cons(h, Cons(h2, t)) if h==h2 => Cons(Cons(h, Cons(h2, Nil())), group(t))
      case Cons(h, Cons(h2, t)) => Cons(Cons(h, Nil()), group(Cons(h2, t)))
      case Cons(h, _) => Cons(Cons(h, Nil()), Nil())
      case _ => Nil()




    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = (filter(s)(pred(_)), filter(s)(!pred(_)))

    def coursesSeq(s: Sequence[Person]): Sequence[String] = map(filter(s)(p=>(!isStudent(p)) ))(t=> course(t))



  end Sequence
end Sequences

@main def trySequences =
  import Sequences.*
  import u02.Modules.*

  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) // 21+31 = 52
  val teac: Sequence.Cons[Person] =
    Cons(Person.Teacher("Primo", "mate"),
      Cons(Person.Student("Secondo", 12),
        Cons(Person.Teacher("Terzo", "ita"),
          Nil())))
  println(coursesSeq(teac))

  //



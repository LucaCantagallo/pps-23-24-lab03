package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*
import u03.Lab03.SequenceExtension.*


class Lab03Test:
  import u03.Lab03.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
  
  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))
  
  @Test def testZip() = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))
    
  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Just(10), min(Cons(20, Cons(10, Cons(30, Nil())))))
    assertEquals(Empty(), min(Nil()))

//task 2

  @Test def testIsStudent() =
    assertTrue(isStudent(Person.Student("Luca", 1999)))
    assertFalse(isStudent(Person.Teacher("Mirko", "PPS")))

  @Test def testGetCourses() =
    assertEquals(Cons("PPS", Cons("PCD", Nil())), coursesList(Cons(Person.Student("Luca", 1999), Cons(Person.Teacher("Mirko", "PPS"), Cons(Person.Teacher("Alessandro", "PCD"), Nil())))))
    assertEquals(Nil(), coursesList(Cons(Person.Student("Luca", 1999), Nil())))
    assertNotEquals(Nil(), coursesList(Cons(Person.Teacher("Mirko", "PPS"), Nil())))

  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))

//extension

  import SequenceExtension.*

  @Test def testExtensions() =
    val s = Cons(10, Cons(20, Cons(30, Nil())))
    assertEquals(60, s.sumExt())
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.mapExt[Int](_ + 1))
    assertEquals(Cons(20, Cons(30, Nil())), l.filterExt(_ >= 20))
    assertEquals(Cons(10, Cons(20, Nil())), l.takeExt(2))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), l.zipExt[String](Cons("10", Cons("20", Cons("30", Nil())))))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), l.concatExt(Cons(40, Cons(50, Nil()))))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.flatMapExt(v => Cons(v + 1, Nil())))
    assertEquals(Just(10), l.minExt())
    assertEquals(-16, Cons(3, Cons(7, Cons(1, Cons(5, Nil())))).foldLeftExt()(0)(_ - _) )
    assertEquals(Cons("PPS", Cons("PCD", Nil())), Cons(Person.Student("Luca", 1999), Cons(Person.Teacher("Mirko", "PPS"), Cons(Person.Teacher("Alessandro", "PCD"), Nil()))).coursesListExt())





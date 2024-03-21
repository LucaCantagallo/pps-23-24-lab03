package u02

import org.junit.*
import org.junit.Assert.*

class TuplesTest:

  val tuple = ("a", 10)

  @Test def testMatches() =
    assertTrue(tuple match {case (x, 10) => true; case _ => false})
    assertTrue: // significant indentation style
      tuple match 
        case ("a", 10) => true
        case _ => false
    
  @Test def testEquality() =
    assertEquals(tuple, Tuple2("a", 10))
    assertNotEquals(tuple, ("a", 11))

  @Test def testCheckFirstComponent() =
    assertTrue(Tuples.checkFirstComponent(Tuples.Tup2("a", 10), "a"))

  @Test def testSwitch() =
    def switch[A, B](t: (A, B)): (B, A) = t match 
      case (a, b) => (b, a)  
    assertEquals(("a", 10), switch((10, "a")))  

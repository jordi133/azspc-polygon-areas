package polygonareas

import org.scalatest.WordSpec
import polygonalareas.{AnglesSet, Vector2D}

/**
  * Created by Jordi on 16-12-2016.
  */
class AnglesSetTest extends WordSpec {
  implicit def tupleToVector2D(tuple :(Int, Int)): Vector2D = Vector2D(tuple._1, tuple._2)

  "contains" should {
    "return false on empty angleset" in {
      val as = AnglesSet.empty
      assert(as.contains((0,0)) === false)
      assert(as.contains((1,0)) === false)
      assert(as.contains((1,-1)) === false)
    }
    "return true after adding a value" in {
      val as = AnglesSet.empty.put((1,2))
      assert(as.contains((0,0)) === false)
      assert(as.contains((1,0)) === false)
      assert(as.contains((1,-1)) === false)
      assert(as.contains((1,2)) === true)
    }
    "return true after adding a value with the same angle" in {
      val as = AnglesSet.empty.put((1,2))
      assert(as.contains((0,0)) === false)
      assert(as.contains((1,0)) === false)
      assert(as.contains((1,-1)) === false)
      assert(as.contains((2,4)) === true)
    }
    "return true after adding a value with the same angle with negative components" in {
      val as = AnglesSet.empty.put((1,-2)).put(-1,0)
      assert(as.contains((0,0)) === false)
      assert(as.contains((0,1)) === false)
      assert(as.contains((1,0)) === true)
      assert(as.contains((-3,0)) === true)
      assert(as.contains((-1,2)) === true)
      assert(as.contains((2,-4)) === true)
    }
  }

}

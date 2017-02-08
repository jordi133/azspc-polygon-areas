package polygonareas.core

import org.scalatest.WordSpec
import polygonalareas.core.AnglesSet
import polygonalareas.{Point, Vector2D}

/**
  * Created by Jordi on 16-12-2016.
  */
class AnglesSetTest extends WordSpec {
  implicit def tupleToVector2D(tuple: (Int, Int)): Vector2D = Vector2D(tuple._1, tuple._2)

  "contains" should {
    "return false on empty angleset" in {
      val as = AnglesSet.empty
      assert(as.contains((0, 0)) === false)
      assert(as.contains((1, 0)) === false)
      assert(as.contains((1, -1)) === false)
    }
    "return true after adding a value" in {
      val as = AnglesSet.empty.put((1, 2))
      assert(as.contains((0, 0)) === false)
      assert(as.contains((1, 0)) === false)
      assert(as.contains((1, -1)) === false)
      assert(as.contains((1, 2)) === true)
    }
    "return true after adding a value with the same angle" in {
      val as = AnglesSet.empty.put((1, 2))
      assert(as.contains((0, 0)) === false)
      assert(as.contains((1, 0)) === false)
      assert(as.contains((1, -1)) === false)
      assert(as.contains((2, 4)) === true)
    }
    "return true after adding a value with the same angle with negative components" in {
      val as = AnglesSet.empty.put((1, -2)).put(-1, 0)
      assert(as.contains((0, 0)) === false)
      assert(as.contains((0, 1)) === false)
      assert(as.contains((1, 0)) === true)
      assert(as.contains((-3, 0)) === true)
      assert(as.contains((-1, 2)) === true)
      assert(as.contains((2, -4)) === true)
    }
  }

  "fromPoints" should {
    "return the anglesset for those poitns" in {
      val points = Vector(Point(0, 0), Point(1, 0), Point(0, 1))
      val angles = AnglesSet.fromPoints(points)
      println(s"angles: $angles")
      assert(angles.contains(Vector2D(-1, 1)))
      assert(angles.contains(Vector2D(0, 1)))
      assert(angles.contains(Vector2D(1, 0)))
      assert(angles.size === 3)
    }
  }

  "bug" should {
    "be fixed" in {
      val points = Vector(Point(3, 4), Point(2, 40), Point(1, 46), Point(47, 47), Point(46, 1), Point(20, 2), Point(7, 5), Point(12, 11), Point(11, 6), Point(22, 18), Point(14, 7), Point(23, 19), Point(24, 17), Point(32, 16), Point(33, 27), Point(35, 30), Point(44, 37), Point(40, 3), Point(45, 41), Point(36, 45), Point(42, 44), Point(43, 43), Point(41, 42), Point(25, 39), Point(39, 38), Point(34, 36), Point(38, 35), Point(28, 34), Point(18, 31), Point(37, 33), Point(29, 32), Point(31, 29), Point(30, 28), Point(27, 26), Point(16, 24), Point(4, 25), Point(21, 23), Point(26, 22), Point(17, 21), Point(8, 13), Point(19, 20), Point(15, 15), Point(10, 14), Point(13, 12), Point(5, 10), Point(9, 8), Point(6, 9))
//      assert(!Polygon(points.toArray).isSelfIntersecting)
      var angles = AnglesSet.empty
      for (i <- points.indices) {
        val v = points((i + 1) % points.length) - points(i)
        if (angles.contains(v)) println(s"duplicate at ${points(i)}, ${points((i + 1) % points.length)}: $v")
        angles = angles.put(v)

        // TODO check on intersection instead of parallel edges :)
      }
    }
  }

}

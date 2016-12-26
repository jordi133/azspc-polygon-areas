package polygonareas

import org.scalatest.WordSpec
import polygonalareas.Implicits.LineSegmentOps
import polygonalareas.{LineSegment, Point}

/**
  * Created by Jordi on 14-12-2016.
  */
class ImplicitsTest extends WordSpec {

  implicit def tupleToLineSegment(tuple: ((Int, Int), (Int, Int))): LineSegment =
    LineSegment(Point(tuple._1._1, tuple._1._2), Point(tuple._2._1, tuple._2._2))

  "linesIntersect" should {
    "return true for (0,0)-(1,1) and (0,1)-(1,0)" in {
      val l1: LineSegment = ((0, 0), (1, 1))
      val l2: LineSegment = ((0, 1), (1, 0))
      assert((l1 intersects l2) === true)
    }
    "return true for (0,0)-(0,1) and (0,1)-(1,0)" in {
      val l1: LineSegment = ((0, 0), (1, 1))
      val l2: LineSegment = ((0, 1), (1, 0))
      assert((l1 intersects l2) === true)
    }
    "return false for (0,0)-(1,0) and (0,1)-(1,1)" in {
      val l1: LineSegment = ((0, 0), (1, 0))
      val l2: LineSegment = ((0, 1), (1, 1))
      assert((l1 intersects l2) === false)
    }
  }

}

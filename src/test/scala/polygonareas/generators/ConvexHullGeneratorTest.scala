package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.Point
import polygonalareas.generators.ConvexHullGenerator

/**
  * Created by Jordi on 26-12-2016.
  */
class ConvexHullGeneratorTest extends WordSpec {

  val gen = ConvexHullGenerator

  implicit def tupleToPoint(tuple: (Int, Int)): Point = Point(tuple._1, tuple._2)

  "getConvexHullFromSortedPoints" should {
    "generate correct convex hull for a square" in {
      val points: IndexedSeq[Point] = Vector((0, 1), (1, 4), (2, 3), (3, 0), (4, 2), (5, 2))
      assert(gen.getConvexHullFromSortedPoints(points) === (List(Point(0, 1), Point(3, 0), Point(5, 2), Point(1, 4)).reverse, Set(Point(2, 3), Point(4, 2))))
    }
    "return all points after creating the convex hull" in {
      val points: IndexedSeq[Point] = Vector((0, 1), (1, 4), (2, 3), (3, 0), (4, 2), (5, 2))
      val (convexHull, rest) = gen.getConvexHullFromSortedPoints(points)
      assert(convexHull.size + rest.size === points.size)
    }
  }

}

package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.Point
import polygonalareas.generators.{PointGenerator, PolygonGenerator}
import polygonalareas.genetic.{Mutater, Polygon}

/**
  * Created by Jordi on 18-2-2017.
  */
class MutaterTest extends WordSpec {

  "pickSomeExp" should {
    "pick some indices" in {
      for (_ <- 1 to 10) {
        val indices = 1 to 100
        val result = Mutater.pickSomeExp(indices, 10)
        println(result.sorted)
        assert(result.size === 10)
        assert(result.forall(indices.contains))
      }
    }
  }
  "triangulatePointsOnSurroundingArea" should {
    "sort points with those with the smallest angle ending up last for three points" in {
      val points = Vector(Point(1, 1), Point(100, 2), Point(2, 3))
      val result = Mutater.triangulatePointsOnSurroundingArea(Polygon(points))
      println(result)
      assert(result.last === 1)
    }
    "sort points with those with the smallest angle ending up last for many points" in {
      val points = Vector(Point(8, 1), Point(11, 3), Point(7, 4), Point(9, 7), Point(6, 8), Point(5, 6), Point(1, 5), Point(3, 2))
      val result = Mutater.triangulatePointsOnSurroundingArea(Polygon(points))
      println(result)
      assert(result.tail.tail.toList === List(3, 7, 6, 0, 2, 1))
    }

  }

  "sortPointsOnSurroundingArea" should {
    "give a roughly correct sorting" in {
      val points = Vector(Point(8, 1), Point(11, 3), Point(7, 4), Point(9, 7), Point(6, 8), Point(5, 6), Point(1, 5), Point(3, 2))
      val result = Mutater.sortPointsOnSurroundingArea(Polygon(points))
      println(result)
      assert(result.tail.tail.toList === List(3, 7, 6, 0, 2, 1))
    }

  }

}

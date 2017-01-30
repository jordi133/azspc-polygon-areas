package polygonareas.genetic

import org.scalatest.WordSpec
import polygonalareas.generators.PointGenerator
import polygonalareas.genetic.PolygonGenerator
import polygonalareas.{Point, Polygon}

/**
  * Created by Jordi on 29-1-2017.
  */
class PolygonGeneratorTest extends WordSpec{

  "putPointsAt12and21" should {
//    "work" in {
//      val points = Vector(Point(1,1), Point(2,3), Point(4,5),Point(4,2),Point(3,4))
//      val result = PolygonGenerator.putPointsAt12and21(points)
//      assert(result(0) === Point(1,2))
//      assert(result(1) === Point(2,1))
//      assert(result.map(_.x).toSet === points.map(_.x).toSet)
//      assert(result.map(_.y).toSet === points.map(_.y).toSet)
//    }

    "test" in {
      val n = 10
      for (i <- 1 to 100) {
        val polygon = PolygonGenerator.generatePolygonInSquare(n)(PointGenerator.generateDoubleDiagonalPoints((2 * Math.pow(n, 0.5)).toInt,seed = Some(1)))()
        println(polygon)
        assert(!Polygon(polygon.toArray).isSelfIntersecting)
      }
    }
  }

  "polygonGenerator" should {
    "give unique coordinates" in {
      val n = 100
      for (i <- 1 to 100) {
        val polygon = PolygonGenerator.generatePolygonInSquare(n)(PointGenerator.generateDiagonalPoints((2 * Math.pow(n, 0.5)).toInt,seed = Some(1)))()
        println(polygon)
        println((1 to 100).filter(!polygon.map(_.x).contains(_)))
        assert(polygon.map(_.x).distinct.size === n)
        assert(polygon.map(_.y).distinct.size === n)
      }

    }
  }

}

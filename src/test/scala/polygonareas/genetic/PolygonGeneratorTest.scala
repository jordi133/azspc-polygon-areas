package polygonareas.genetic

import org.scalatest.WordSpec
import polygonalareas.genetic.{PointGenerator, PolygonGenerator}
import polygonalareas.{Point, Polygon}

/**
  * Created by Jordi on 29-1-2017.
  */
class PolygonGeneratorTest extends WordSpec{

  "polygonGenerator" should {
    "give unique coordinates" in {
      val n = 100
      for (i <- 1 to 100) {
        val polygon = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(PointGenerator.generateRandomPoints()))(n)
        println(polygon)
        println((1 to 100).filter(!polygon.map(_.x).contains(_)))
        assert(polygon.map(_.x).distinct.size === n)
        assert(polygon.map(_.y).distinct.size === n)
      }

    }
  }

  "generateStarPolygon" should {
    "generate valid polygons" in {
      val polygon = PolygonGenerator.generateStarPolygon(PointGenerator.generateDiagonalPoints(spread = 4))(100)
      println(polygon)
    }
  }

}

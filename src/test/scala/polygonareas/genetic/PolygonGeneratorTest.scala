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

  "createFractalFromNarrowDiagonal" should {
    "generate a valid polygon for n=15" in {
      val pg = PolygonGenerator.createFractalFromNarrowDiagonal(15)
      println(pg)
      assert(pg.size == 15)
    }
    "generate a valid polygon for n=13" in {
      val pg = PolygonGenerator.createFractalFromNarrowDiagonal(13)
      println(pg)
      assert(pg.size == 13)
    }
  }

}

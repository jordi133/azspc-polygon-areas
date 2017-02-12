package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.Point
import polygonalareas.generators.{PointGenerator, PolygonGenerator}

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
    "generate a valid polygon for n=101" in {
      val pg = PolygonGenerator.createFractalFromNarrowDiagonal(101)
      println(pg)
      assert(pg.size == 101)
    }
  }
  "generateReverseStarPolygon" should {
    "generate a valid polygon with PointGenerator.generateRandomPoints for n=105" in {
      val pg = PolygonGenerator.generateReverseStarPolygon(PointGenerator.generateRandomPoints())(105)
      println(pg)
    }
    "generate a valid polygon in a square for n=105" in {
      val pg = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateReverseStarPolygon(PointGenerator.generateRandomPoints()))(105)
      println(pg)
    }
  }
  "triangleBasedGeneratorSurfaceBased" should {
    "generate a valid polygon with PointGenerator.generateRandomPoints for n=105" in {
      val pg = PolygonGenerator.triangleBasedGeneratorSurfaceBased(PointGenerator.generateRandomPoints())(105)
      println(pg)
    }
    "generate a valid polygon in a square for n=105" in {
      val pg = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.triangleBasedGeneratorSurfaceBased(PointGenerator.generateRandomPoints()))(105)
      println(pg)
    }
    "generate a valid polygon with combine for n=105" in {
      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pg2: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(pg1, pg2)(n)
      val pg = PolygonGenerator.triangleBasedGeneratorSurfaceBased(pointGenerator)
      println(pg)
    }
  }
  "triangleBasedGeneratorSqrPeripheryBased" should {
    "generate a valid polygon with PointGenerator.generateRandomPoints for n=105" in {
      val pg = PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(PointGenerator.generateRandomPoints())(105)
      println(pg)
    }
    "generate a valid polygon in a square for n=105" in {
      val pg = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(PointGenerator.generateRandomPoints()))(105)
      println(pg)
    }
    "generate a valid polygon with combine for n=105" in {
      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pg2: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(pg1, pg2)(n)
      val pg = PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(pointGenerator)
      println(pg)
    }
  }
  "generateTwoPolygonsInSquare" should {
    "generate a valid polygon with triangleBasedGeneratorSqrPeripheryBased for n=105" in {
      val pg = PolygonGenerator.generateTwoPolygonsInSquare(PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(PointGenerator.generateRandomPoints()))(105)
      println(pg)
    }
  }

}

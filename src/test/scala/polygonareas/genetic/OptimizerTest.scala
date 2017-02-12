package polygonareas.genetic

import org.scalatest.WordSpec
import polygonalareas.core.SolutionManager
import polygonalareas.generators.{PointGenerator, PolygonGenerator}
import polygonalareas.{Point, puzzleSizes}
import polygonalareas.genetic.Optimizer

import scala.util.{Random, Try}

/**
  * Created by Jordi on 5-2-2017.
  */
class OptimizerTest extends WordSpec {
  "optimizeFromPolygon" should {
    "optimize a single size" in {
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
      val n = puzzleSizes.last //(12)
      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pg2: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      def pg3: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.25)).toInt)(n)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(PointGenerator.combine(pg1, pg2), pg3)(n)
      def polygonGeneratorMin = PolygonGenerator.createFractalFromNarrowDiagonal
      //      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.createFractalFromNarrowDiagonal)
      //      def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)
      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(polygonGeneratorMin)
      val optimizer = new Optimizer(
        maxRoundsWithoutImprovement = 150,
        familyRevitalizations = 0,
        familySize = 15,
        nrOfFamilies = 1,
        maxOffSpring = 25
      )
      for (_ <- 1 to 100) optimizer.optimizeFromPolygon(polygonGeneratorMax, n, true)(actionOnFound)
    }

    def rollExponential(d: Double = Random.nextDouble(), acc: Int = 0): Int = {
      if (d > 0.5) acc
      else rollExponential(2 * d, acc + 1)
    }

    "optimize based on opportunity" in {
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
      //      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      //      def pg2: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      //      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(pg1, pg2)(n)
      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pg2: Int => Set[Point] = n => PointGenerator.generateCrossPoints((2 * Math.pow(n, 0.5)).toInt)(n)
      def pg3: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((.5 * Math.pow(n, 0.5)).toInt)(n)
      def pgCross: Int => Set[Point] = n => PointGenerator.generateCrossPoints((2 * Math.pow(n, 0.5)).toInt)(n)
      def pointGenerator: Int => Set[Point] = pgCross //n => PointGenerator.combine(PointGenerator.combine(pg1, pg2), pg3)(n)
      //      def polygonGeneratorMin = PolygonGenerator.generateReverseStarPolygon(pointGenerator)
      //      def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)
      def polygonGeneratorMin = PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(pointGenerator)
      def polygonGeneratorMax = PolygonGenerator.generateTwoPolygonsInSquare(polygonGeneratorMin)
//      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(polygonGeneratorMin)
      for (i <- 1 to 1000) {
        val sizes = SolutionManager.opportunities
        val n = sizes(Math.min(rollExponential(), sizes.length - 1))
        val optimizer = new Optimizer(
          maxRoundsWithoutImprovement = 4 * (50 - Math.sqrt(n)).toInt,
          familyRevitalizations = 0,
          familySize = 15,
          nrOfFamilies = 1,
          maxOffSpring = 25
        )
        Try(optimizer.optimizeFromPolygon(polygonGeneratorMax, n, true)(actionOnFound))
        Try(optimizer.optimizeFromPolygon(polygonGeneratorMin, n, false)(actionOnFound))
      }
    }
    "optimize small values based on opportunity" in {
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
      //      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      //      def pg2: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      //      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(pg1, pg2)(n)
      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pg2: Int => Set[Point] = n => PointGenerator.generateCrossPoints((1.5 * Math.pow(n, 0.6)).toInt)(n)
      def pg3: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.2)).toInt)(n)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(PointGenerator.combine(pg1, pg2), pg3)(n)
      def polygonGeneratorMin = PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(pg2)//pointGenerator)
//      def polygonGeneratorMin = PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(pointGenerator)
      //      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateReverseStarPolygon(pointGenerator))
      //      def polygonGeneratorMin = PolygonGenerator.generateReverseStarPolygon(pointGenerator)
      //      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.triangleBasedGenerator(pointGenerator))
      //      def polygonGeneratorMin = PolygonGenerator.triangleBasedGenerator(pointGenerator)
      def polygonGeneratorMax = PolygonGenerator.generateTwoPolygonsInSquare(polygonGeneratorMin)
//      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(polygonGeneratorMin)
      for (i <- 1 to 1000) {
        val sizes = SolutionManager.opportunities.filter(_ < 150)
        val n = sizes(Math.min(rollExponential(), sizes.length - 1))
        val optimizer = new Optimizer(
          maxRoundsWithoutImprovement = 2 * (75 - Math.sqrt(n)).toInt,
          familyRevitalizations = 0,
          familySize = 25,
          nrOfFamilies = 1,
          maxOffSpring = 50
        )
        Try(optimizer.optimizeFromPolygon(polygonGeneratorMax, n, true)(actionOnFound))
        Try(optimizer.optimizeFromPolygon(polygonGeneratorMin, n, false)(actionOnFound))
      }
    }
  }
}

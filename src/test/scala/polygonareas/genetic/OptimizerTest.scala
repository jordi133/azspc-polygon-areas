package polygonareas.genetic

import org.scalatest.WordSpec
import polygonalareas.core.SolutionManager
import polygonalareas.generators.{PointGenerator, PolygonGenerator}
import polygonalareas.{Point, puzzleSizes}
import polygonalareas.genetic.Optimizer

import scala.util.Random

/**
  * Created by Jordi on 5-2-2017.
  */
class OptimizerTest extends WordSpec {
  "optimizeFromPolygon" should {
    "optimize a single size" in {
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
      val n = puzzleSizes(12)
      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pg2: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(pg1, pg2)(n)
      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.createFractalFromNarrowDiagonal)
//      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))
      def polygonGeneratorMin = PolygonGenerator.createFractalFromNarrowDiagonal
//      def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)
      val optimizer = new Optimizer(
        maxRoundsWithoutImprovement = 150,
        familyRevitalizations = 0,
        familySize = 15,
        nrOfFamilies = 1,
        maxOffSpring = 25
      )
      for (_ <- 1 to 100)       optimizer.optimizeFromPolygon(polygonGeneratorMax, n, true)(actionOnFound)
    }

    def rollExponential(d: Double = Random.nextDouble(), acc: Int = 0): Int = {
      if (d > 0.5) acc
      else rollExponential(2 * d, acc + 1)
    }

    "optimize based on opportunity" in {
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pg2: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(pg1, pg2)(n)
      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))
      def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)
      for (i <- 1 to 1000) {
        val n = SolutionManager.opportunities(rollExponential())
        val optimizer = new Optimizer(
          maxRoundsWithoutImprovement = 100,
          familyRevitalizations = 0,
          familySize = 10,
          nrOfFamilies = 1,
          maxOffSpring = 25
        )
        optimizer.optimizeFromPolygon(polygonGeneratorMax, n, true)(actionOnFound)
        optimizer.optimizeFromPolygon(polygonGeneratorMin, n, false)(actionOnFound)
      }

    }
  }

}

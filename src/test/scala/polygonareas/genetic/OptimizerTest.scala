package polygonareas.genetic

import org.scalatest.WordSpec
import polygonalareas.{Point, SolutionManager, puzzleSizes}
import polygonalareas.genetic.{Optimizer, PointGenerator, PolygonGenerator}

import scala.util.Random

/**
  * Created by Jordi on 5-2-2017.
  */
class OptimizerTest extends WordSpec {
  "optimizeFromPolygon" should {
    "optimize a single size" in {
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
      val n = puzzleSizes(10)
      def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pg2: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(pg1, pg2)(n)
      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))
      def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)
      val optimizer = new Optimizer(
        maxRoundsWithoutImprovement = 25,
        familyRevitalizations = 1000,
        familySize = 10,
        nrOfFamilies = 4,
        maxOffSpring = 25
      )
      optimizer.optimizeFromPolygon(polygonGeneratorMax, n, true)(actionOnFound)
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

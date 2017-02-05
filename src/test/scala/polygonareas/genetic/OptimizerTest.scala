package polygonareas.genetic

import org.scalatest.WordSpec
import polygonalareas.{Point, SolutionManager, puzzleSizes}
import polygonalareas.genetic.{Optimizer, PolygonGenerator, PointGenerator}

/**
  * Created by Jordi on 5-2-2017.
  */
class OptimizerTest extends WordSpec {
  "optimizeFromPolygon" should {
    "optimize a single size" in {
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
      val optimizer = new Optimizer(
        maxRoundsWithoutImprovement = 25,
        familyRevitalizations = 1000,
        familySize = 10,
        nrOfFamilies = 4,
        maxOffSpring = 25
      )
      val n = puzzleSizes(4)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))
      def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)
      optimizer.optimizeFromPolygon(polygonGeneratorMax, n, true)(actionOnFound)
    }
    "optimize based on opportunity" in {
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
//      def pointGenerator: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
      def pointGenerator: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((2 * Math.pow(n, 0.5)).toInt)(n)
//      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateStarPolygon(pointGenerator))
//      def polygonGeneratorMin = PolygonGenerator.generateStarPolygon(pointGenerator)
      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))
      def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)
      for (i <- 1 to 1000) {
        val n = SolutionManager.opportunities.filter(_ < 250).head
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

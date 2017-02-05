package polygonareas.polygonfixer

import org.scalatest.WordSpec
import polygonalareas.{Point, Polygon, SolutionManager, puzzleSizes}
import polygonalareas.generators.PointGenerator
import polygonalareas.genetic.PolygonGenerator
import polygonalareas.polygonfixer.PolygonFixer

import scala.util.Random

/**
  * Created by Jordi on 21-1-2017.
  */
class PolygonFixerTest extends WordSpec {

  def rollExponential(d: Double = Random.nextDouble(), acc: Int = 0): Int = {
    if (d > 0.5) acc
    else rollExponential(2 * d, acc + 1)
  }

  def pointGenerator(n: Int) = if (Random.nextDouble() < 0.5) PointGenerator.generateDoubleDiagonalPoints((2 * Math.pow(n, 0.5)).toInt)(n)
  else if (Random.nextDouble() < 0.5) PointGenerator.generateCircularPoints()(n)
  else PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)

  "PolygonFixer" should {
    "find best opportunity using families" in {
      val fixer = new PolygonFixer(offspringOnGoodPolygon = 20)
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }

      for (i <- 1 to 500) {
        //                val n = SolutionManager.opportunities.filter(_ < 40).head
        val n = SolutionManager.opportunities.head
        //        val n = SolutionManager.opportunities(Math.min(rollExponential(), puzzleSizes.length - 3))
        println(s"n=$n")
        //        fixer.optimizeWithFamiliesFromPoints(pointGenerator(n), false)(actionOnFound)
        //        fixer.optimizeWithFamiliesFromPoints(pointGenerator(n), true)(actionOnFound)
        //        fixer.optimizeWithFamilies(PointGenerator.generateDiagonalPoints(n, (1.5 * Math.pow(n, 0.5)).toInt), false)(actionOnFound)
        //        fixer.optimizeWithFamilies(PointGenerator.generateCircularPoints(n), true)(actionOnFound)
        def polygonGenerator = PolygonGenerator.generatePolygonInSquare(PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt), PolygonGenerator.generateDiagonalPolygon)(n)
        fixer.optimizeWithFamiliesFromPolygon(polygonGenerator,
          maximize = true,
          nrOfFamilies = 1,
          familyRevitalizations = 0,
          familySize = 10)(actionOnFound)
      }
    }
//
//    "find best based on generated polygons for single n" in {
//      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
//      val fixer = new PolygonFixer(offspringOnGoodPolygon = 50)
//      val n = puzzleSizes.last
//      def pointGenerator: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)().toSet
//      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))
//      def polygonGeneratorMin = () => PolygonGenerator.generateWedgePolygon(pointGenerator)
//      fixer.optimizeWithFamiliesFromPolygon(polygonGeneratorMax,
//        maximize = true,
//        nrOfFamilies = 4,
//        familyRevitalizations = 0,
//        maxRoundsWithoutImprovement = 100,
//        familySize = 10)(actionOnFound)
//    }
//    "find best based on generated polygons" in {
//      val fixer = new PolygonFixer(offspringOnGoodPolygon = 10)
//      //      val actionOnFound: IndexedSeq[Point] => Unit = { points => assert(!Polygon(points.toArray).isSelfIntersecting) }
//      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
//
//      for (i <- 1 to 100) {
//                val n = SolutionManager.opportunities.filter(_ < 300).head
////        val n = SolutionManager.opportunities.head
//        //        val n = SolutionManager.opportunities(Math.min(rollExponential(), puzzleSizes.length - 3))
//        println(s"n=$n")
//        implicit val random = new Random(i)
//        //        def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateSimpleDiagonal)(n)
//        //        def polygonGeneratorMin = () => PolygonGenerator.generateSimpleDiagonal(n).toIndexedSeq
//        //        def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt), PolygonGenerator.generateQuarterStarPolygon)(n)
//        //        def polygonGeneratorMin = () => PolygonGenerator.generateQuarterStarPolygon(PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)().toSet)
//
//        def pointGenerator: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)().toSet
//        def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))(n)
//        def polygonGeneratorMin = () => PolygonGenerator.generateWedgePolygon(pointGenerator)(n)
//
//
//        fixer.optimizeWithFamiliesFromPolygon(polygonGeneratorMin,
//          maximize = false,
//          nrOfFamilies = 4,
//          familyRevitalizations = 0,
//          maxRoundsWithoutImprovement = 100,
//          familySize = 10)(actionOnFound)
//        fixer.optimizeWithFamiliesFromPolygon(polygonGeneratorMax,
//          maximize = true,
//          nrOfFamilies = 4,
//          familyRevitalizations = 0,
//          maxRoundsWithoutImprovement = 100,
//          familySize = 10)(actionOnFound)
//      }
//    }
  }


}

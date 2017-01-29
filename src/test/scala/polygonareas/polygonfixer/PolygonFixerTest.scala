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

  "evolutionaryFix" should {
    "yield polygons" in {
      val n = puzzleSizes(11)
      println(s"n=$n")
      val fixer = new PolygonFixer(0)
      val actionOnFound: IndexedSeq[Point] => Unit = { points =>
        SolutionManager.addSolution(points)
      }

      fixer.evolutionaryFix(PointGenerator.generateRandomPoints(n), true)(actionOnFound)
    }

    "find all" in {
      val fixer = new PolygonFixer(0, offspringOnGoodPolygon = 25)
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }

      for (n <- puzzleSizes.drop(10)) {
        println(s"n=$n")
        //        fixer.evolutionaryFix(PointGenerator.generateDiagonalPoints(n, Math.log(n).toInt), false, popsize = 2500 / n)(actionOnFound)
        //        fixer.evolutionaryFix(PointGenerator.generateCircularPoints(n), true, popsize = 2500 / n)(actionOnFound)
        fixer.evolutionaryFix(PointGenerator.generateDiagonalPoints(5 + Math.pow(n, 0.5).toInt)(n), false, popsize = 10)(actionOnFound)
        fixer.evolutionaryFix(PointGenerator.generateCircularPoints()(n), true, popsize = 10)(actionOnFound)
      }
    }
    "find best opportunity" in {
      val fixer = new PolygonFixer(offspringOnGoodPolygon = 50)
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }

      for (i <- 1 to 100) {
        //        val n = SolutionManager.opportunities.filter(_ < 300).head
        val n = SolutionManager.opportunities.head
        println(s"n=$n")
        fixer.evolutionaryFix(PointGenerator.generateCrossPoints((1.5 * Math.pow(n, 0.5)).toInt)(n), false, popsize = 10)(actionOnFound)
        fixer.evolutionaryFix(PointGenerator.generateCircularPoints()(n), true, popsize = 10)(actionOnFound)
      }
    }

    def getIndex(d: Double, acc: Int = 0): Int = {
      if (d > 0.5) acc
      else getIndex(2 * d, acc + 1)
    }

    def pointGenerator(n: Int) = if (Random.nextDouble() < 0.5) PointGenerator.generateDoubleDiagonalPoints((2 * Math.pow(n, 0.5)).toInt)(n)
    else if (Random.nextDouble() < 0.5) PointGenerator.generateCircularPoints()(n)
    else PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)

    "find best opportunity using families" in {
      val fixer = new PolygonFixer(offspringOnGoodPolygon = 20)
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }

      for (i <- 1 to 500) {
        //                val n = SolutionManager.opportunities.filter(_ < 40).head
        //                val n = SolutionManager.opportunities.head
        val n = SolutionManager.opportunities(Math.min(getIndex(Random.nextDouble()), puzzleSizes.length - 1))
        println(s"n=$n")
        //        fixer.optimizeWithFamiliesFromPoints(pointGenerator(n), false)(actionOnFound)
        //        fixer.optimizeWithFamiliesFromPoints(pointGenerator(n), true)(actionOnFound)
        //        fixer.optimizeWithFamilies(PointGenerator.generateDiagonalPoints(n, (1.5 * Math.pow(n, 0.5)).toInt), false)(actionOnFound)
        //        fixer.optimizeWithFamilies(PointGenerator.generateCircularPoints(n), true)(actionOnFound)
        def polygonGenerator = PolygonGenerator.generatePolygonInSquare(n)(PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt))
        fixer.optimizeWithFamiliesFromPolygon(polygonGenerator,
          maximize = true,
          nrOfFamilies = 1,
          familyRevitalizations = 0,
          familySize = 10)(actionOnFound)
      }
    }

    "test new mutations" in {
      val fixer = new PolygonFixer(offspringOnGoodPolygon = 20)
      val actionOnFound: IndexedSeq[Point] => Unit = { points => assert(!Polygon(points.toArray).isSelfIntersecting) }
//      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }

      for (i <- 1 to 5) {
                        val n = SolutionManager.opportunities.filter(_ < 100).head
        //                val n = SolutionManager.opportunities.head
//        val n = SolutionManager.opportunities(Math.min(getIndex(Random.nextDouble()), puzzleSizes.length - 1))
        println(s"n=$n")
        //        fixer.optimizeWithFamiliesFromPoints(pointGenerator(n), false)(actionOnFound)
        //        fixer.optimizeWithFamiliesFromPoints(pointGenerator(n), true)(actionOnFound)
        //        fixer.optimizeWithFamiliesFromPoints(PointGenerator.generateDiagonalPoints(n, (1.5 * Math.pow(n, 0.5)).toInt), false)(actionOnFound)
        def polygonGenerator = PolygonGenerator.generatePolygonInSquare(n)(PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt))
        fixer.optimizeWithFamiliesFromPolygon(polygonGenerator,
          maximize = true,
          nrOfFamilies = 1,
          familyRevitalizations = 0,
          familySize = 10)(actionOnFound)
      }
    }
  }

}

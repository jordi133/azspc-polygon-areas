package polygonareas.polygonfixer

import org.scalatest.WordSpec
import polygonalareas.{Point, Polygon, SolutionManager, puzzleSizes}
import polygonalareas.generators.PointGenerator
import polygonalareas.polygonfixer.PolygonFixer

/**
  * Created by Jordi on 21-1-2017.
  */
class PolygonFixerTest extends WordSpec {
//  "PolygonFixer" should {
//    "create polygons" in {
//      val n = puzzleSizes(3)
//      println(s"n=$n")
//      val points = PointGenerator.generateRandomPoints(n)
//      val fixer = new PolygonFixer(0)
//      val polygons = fixer.fixPolygonFromPoints(points)
//
//      for (p <- polygons) {
//        println(s"polygon found: $p")
//        val polygon = Polygon(p.toArray)
//        assert(!polygon.isSelfIntersecting)
//        assert(polygon.angles.size === n)
//        SolutionManager.addSolution(polygon)
//      }
//    }
//  }
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
        fixer.evolutionaryFix(PointGenerator.generateDiagonalPoints(n, 5 + Math.pow(n, 0.5).toInt), false, popsize = 10)(actionOnFound)
        fixer.evolutionaryFix(PointGenerator.generateCircularPoints(n), true, popsize = 10)(actionOnFound)
      }
    }
    "find best opportunity" in {
      val fixer = new PolygonFixer(offspringOnGoodPolygon = 20)
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }

      for (i <- 1 to 50) {
        val n = SolutionManager.opportunities.filter(_ < 400).head
//        val n = SolutionManager.opportunities.head
        println(s"n=$n")
        fixer.evolutionaryFix(PointGenerator.generateCrossPoints(n, (1.5 * Math.pow(n, 0.5)).toInt), false, popsize = 10)(actionOnFound)
        fixer.evolutionaryFix(PointGenerator.generateCircularPoints(n), true, popsize = 10)(actionOnFound)
      }
    }
  }

}

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
      val points = PointGenerator.generateRandomPoints(n)
      val fixer = new PolygonFixer(0)
      val actionOnFound: IndexedSeq[Point] => Unit = { points =>
        SolutionManager.addSolution(Polygon(points.toArray))
      }

      fixer.evolutionaryFix(points, true)(actionOnFound)
    }

    "find all" in {
      val fixer = new PolygonFixer(0, offspringOnGoodPolygon = 10)
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(Polygon(points.toArray)) }

      for (n <- puzzleSizes.drop(0)) {
        println(s"n=$n")
        val points = PointGenerator.generateCrossPoints(n)
        //        val points = PointGenerator.generateRandomPoints(n)
        fixer.evolutionaryFix(points, false, popsize = 2000 / n)(actionOnFound)
        fixer.evolutionaryFix(points, true, popsize = 2000 / n)(actionOnFound)
      }


    }
  }

}

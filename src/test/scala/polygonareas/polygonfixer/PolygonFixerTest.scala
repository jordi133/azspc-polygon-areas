package polygonareas.polygonfixer

import org.scalatest.WordSpec
import polygonalareas.{Point, Polygon, SolutionManager, puzzleSizes}
import polygonalareas.generators.PointGenerator
import polygonalareas.polygonfixer.PolygonFixer

/**
  * Created by Jordi on 21-1-2017.
  */
class PolygonFixerTest extends WordSpec {
  "PolygonFixer" should {
    "create polygons" in {
      val n = puzzleSizes(3)
      println(s"n=$n")
      val points = PointGenerator.generateRandomPoints(n)
      val fixer = new PolygonFixer(0)
      val polygons = fixer.fixPolygonFromPoints(points)

      for (p <- polygons) {
        println(s"polygon found: $p")
        val polygon = Polygon(p.toArray)
        assert(!polygon.isSelfIntersecting)
        assert(polygon.angles.size === n)
        SolutionManager.addSolution(polygon)
      }
    }
  }
  "evolutionaryFix" should {
    "yield polygons" in {
      val n = puzzleSizes.last
      println(s"n=$n")
      val points = PointGenerator.generateRandomPoints(n)
      val fixer = new PolygonFixer(0)
      val actionOnFound: IndexedSeq[Point] => Unit = {points =>
        SolutionManager.addSolution(Polygon(points.toArray))
      }

      fixer.evolutionaryFix(points, true)(actionOnFound)
    }
  }
}

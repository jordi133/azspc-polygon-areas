package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.Point
import polygonalareas.generators.TriangleBasedGenerator
import polygonalareas.genetic.Polygon

/**
  * Created by Jordi on 2-1-2017.
  */
class TriangleBasedGeneratorTest extends WordSpec {


  "TriangleBasedGenerator" should {
    "Generate correct polygons" in {
      val gen = new TriangleBasedGenerator(5, seed = 0)
      val pols = gen.getPolygons(Vector(Point(1, 1), Point(4, 2), Point(5, 4)))
      assert(pols.nonEmpty)
      for (p <- pols) {
        val polygon = Polygon(p)
        println(polygon)
        assert(!polygon.isSelfIntersecting  )
        assert(polygon.angles.size == polygon.size)
      }
    }
  }

  "TriangleConstruct" when {
    val tbg = new TriangleBasedGenerator(5)
    "doubleSurfaceDiffForPoint" should {
      "calculate correct surface diff for clockwise points" in {
        val points = Vector(Point(1, 1), Point(1, 5), Point(5, 1))
        val tc = tbg.TriangleConstruct(points)
        assert(tc.doubleSurfaceDiffForPoint(2, Point(5, 5)) == 16)
        assert(tc.doubleSurfaceDiffForPoint(2, Point(1, 1)) == -16)
        assert(tc.doubleSurfaceDiffForPoint(3, Point(3, 3)) == -8)
      }
      "calculate correct surface diff for counter clockwise points" in {
        val points = Vector(Point(1, 1), Point(2, 1), Point(1, 2))
        val tc = tbg.TriangleConstruct(points)
        assert(tc.doubleSurfaceDiffForPoint(2, Point(2, 2)) == -1)
        assert(tc.doubleSurfaceDiffForPoint(2, Point(1, 1)) == 1)
        assert(tc.doubleSurfaceDiffForPoint(3, Point(-4, 1)) == -5)
      }
    }
    "nextStep" should {
      "generate correct set of next constructs" in {

      }
    }
  }

}

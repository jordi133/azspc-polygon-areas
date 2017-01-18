package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.generators.LineBasedGenerator
import polygonalareas.{Point, Polygon, SolutionManager}
import polygonalareas.puzzleSizes

/**
  * Created by Jordi on 2-1-2017.
  */
class LineBasedGeneratorTest extends WordSpec {

  "LineBasedGenerator" should {
    "Generate correct polygons" in {
      val gen = new LineBasedGenerator(5, seed = 0)
      val pols = gen.getAllPolygons(Point(1, 1))
      assert(pols.nonEmpty)
      for (p <- pols) {
        val polygon = Polygon(p.toArray)
        //        println(polygon)
        assert(!polygon.isSelfIntersecting)
        assert(polygon.angles.size == polygon.size)
      }
      val sorted = pols.toIndexedSeq.map(seq => Polygon(seq.toArray)).sortBy(_.doubleSurface)
      SolutionManager.addSolution(sorted.head)
      SolutionManager.addSolution(sorted.last)
    }

    "Run with all starting points" in {
      val gen = new LineBasedGenerator(puzzleSizes(1), seed = 0)
      val inits = gen.getAllInitialPoints
      val pols = (for (init <- inits.par) yield gen.getAllPolygons(init)).flatten
      //        assert(pols.nonEmpty)
      //        for (p <- pols) {
      //          val polygon = Polygon(p.toArray)
      //          println(polygon)
      //          assert(!polygon.isSelfIntersecting  )
      //          assert(polygon.angles.size == polygon.size)
      //        }
      val sorted = pols.toIndexedSeq.map(seq => Polygon(seq.toArray)).sortBy(_.doubleSurface)
      SolutionManager.addSolution(sorted.head)
      SolutionManager.addSolution(sorted.last)
      SolutionManager.saveToFile()
    }
    "Run with limited population size" in {
      //      for (size <- puzzleSizes) {
      val size = puzzleSizes(4)
      val gen = new LineBasedGenerator(size, seed = 0)
      val pols = gen.generatePolygons(1000)
      //        assert(pols.nonEmpty)
      //        for (p <- pols) {
      //          val polygon = Polygon(p.toArray)
      //          println(polygon)
      //          assert(!polygon.isSelfIntersecting  )
      //          assert(polygon.angles.size == polygon.size)
      //        }
      for (p <- pols) SolutionManager.addSolution(Polygon(p.toArray))
      SolutionManager.saveToFile()
    }

  }


  //  "TriangleConstruct" when {
  //    val tbg = new LineBasedGenerator(5)
  //    "doubleSurfaceDiffForPoint" should {
  //      "calculate correct surface diff for clockwise points" in {
  //        val points = Vector(Point(1, 1), Point(1, 5), Point(5, 1))
  //        val tc = tbg.TriangleConstruct(points)
  //        assert(tc.doubleSurfaceDiffForPoint(2, Point(5, 5)) == 16)
  //        assert(tc.doubleSurfaceDiffForPoint(2, Point(1, 1)) == -16)
  //        assert(tc.doubleSurfaceDiffForPoint(3, Point(3, 3)) == -8)
  //      }
  //      "calculate correct surface diff for counter clockwise points" in {
  //        val points = Vector(Point(1, 1), Point(2, 1), Point(1, 2))
  //        val tc = tbg.TriangleConstruct(points)
  //        assert(tc.doubleSurfaceDiffForPoint(2, Point(2, 2)) == -1)
  //        assert(tc.doubleSurfaceDiffForPoint(2, Point(1, 1)) == 1)
  //        assert(tc.doubleSurfaceDiffForPoint(3, Point(-4, 1)) == -5)
  //      }
  //    }
  //    "nextStep" should {
  //      "generate correct set of next constructs" in {
  //
  //      }
  //    }
  //  }

}

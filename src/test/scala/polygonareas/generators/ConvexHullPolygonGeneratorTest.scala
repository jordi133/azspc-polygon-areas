package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.Point
import polygonalareas.generators.ConvexHullPolygonGenerator

/**
  * Created by Jordi on 27-12-2016.
  */
class ConvexHullPolygonGeneratorTest extends WordSpec {

  val gen = new ConvexHullPolygonGenerator(4)

  "findParallelEdges" should {
    "return all parallel edges of a polygon" in {
      val points = Vector(Point(0,0),Point(1,0),Point(1,1),Point(0,1))
      val par = gen.findParallelEdges(points)
      assert(par === List(List(2, 0), List(3, 1)))
    }
  }
  "liesInTriangle" should {
    "work for inside counterclockwise" in {
      assert(gen.liesInTriangle(Point(1,1), Point(0,0), Point(2,0), Point(1,2)) === true)
    }
    "work for inside clockwise" in {
      assert(gen.liesInTriangle(Point(1,1), Point(0,0), Point(1,2), Point(2,0)) === true)
    }
    "work for outside counterclockwise" in {
      assert(gen.liesInTriangle(Point(1,3), Point(0,0), Point(2,0), Point(1,2)) === false)
    }
    "work for outside clockwise" in {
      assert(gen.liesInTriangle(Point(1,3), Point(2,0), Point(0,0), Point(1,2)) === false)
    }
  }
  "injectingIsAllowed" should {
    "work for trivial case" in {
      val points = IndexedSeq(Point(0,0), Point(0,4), Point(4,4), Point(4,0))
      val rest = Set(Point(1,1), Point(2,2), Point(3,3))
      assert(gen.injectingIsAllowed(points, 0, Point(1,2), rest) === true)
      assert(gen.injectingIsAllowed(points, 1, Point(1,2), rest) === true)
      assert(gen.injectingIsAllowed(points, 2, Point(1,2), rest) === false)
      assert(gen.injectingIsAllowed(points, 3, Point(1,2), rest) === false)
    }
  }
  "fixParallelEdges" should {
    "work for " in {
      val convexHull = IndexedSeq(Point(0,0), Point(0,5), Point(1,4), Point(3,4), Point(4,0))
      val rest = Set(Point(1,1), Point(2,1), Point(1,2), Point(3,1))
      println("result:")
      for ((polygon, rest) <- gen.fixParallelEdges(convexHull, rest)) {
        println(s"polygon: $polygon, rest: $rest")
      }
    }
  }

  "ConvexHullPolygonGenerator" should {
    "work fast for small n" in {
      val generator = new ConvexHullPolygonGenerator(97, 0)
      for (i <- (1 to 1000).par) {
        val result = generator.generatePolygons
        for (r <- result) println(r)
      }
    }
  }
}

package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.{Point, Polygon}
import polygonalareas.generators.{ConvexHullGenerator, PolygonConstruct}

/**
  * Created by Jordi on 29-12-2016.
  */
class PolygonConstructTest extends WordSpec {

  "nextStepRemoveParallelEdge" should {
    "increase the polygon by one point, removing at least 1 parallel edge" in {
      val points = Vector(Point(0,0), Point(2,0), Point(3,5), Point(0,5))
      val rest = Set(Point(1,1), Point(1,4))
      val parallelEdges = Seq(Seq(0,2))
      val pc = PolygonConstruct(points, rest, parallelEdges)

      val nextGen = pc.nextStepRemoveParallelEdge
      assert(nextGen.nonEmpty)
      for (next <- nextGen) {
        println(s"result: $next")
        assert(next.rest.size == rest.size - 1)
        assert(next.points.size == points.size + 1)
        assert(next.parallelEdges.isEmpty)
        assert(!Polygon(next.points.toArray).isSelfIntersecting)
        assert(Polygon(next.points.toArray).angles.getSet.size == next.points.size)
      }
    }
  }
  "nextStepNoParallelEdges" should {
    "increase the polygon by one point, creating at most 1 parallel edge" in {
      val points = Vector(Point(0,0), Point(2,0), Point(3,5), Point(0,4))
      val rest = Set(Point(1,1), Point(1,4))
      val pc = PolygonConstruct(points, rest, Seq())

      val nextGen = pc.nextStepNoParallelEdges
      assert(nextGen.nonEmpty)
      for (next <- nextGen) {
        println(s"result: $next")
        assert(next.rest.size == rest.size - 1)
        assert(next.points.size == points.size + 1)
        assert(next.parallelEdges.size <= 1)
        if (next.parallelEdges.nonEmpty) assert(next.parallelEdges.head.size <= 2)
        if (next.parallelEdges.isEmpty) assert(Polygon(next.points.toArray).angles.getSet.size == next.points.size)
        assert(!Polygon(next.points.toArray).isSelfIntersecting)
      }
    }
  }

  "bugs" should {
    "be fixed" in {
      val bug = PolygonConstruct(Vector(Point(5,1), Point(1,5)),Set(Point(2,4), Point(3,3), Point(4,2)),List(List(1, 0)))
      val step1 = bug.nextStep
      val step2 = step1.flatMap(_.nextStep)
      val step3 = step2.flatMap(_.nextStep)
      step3.foreach{ pc =>
        val polygon = Polygon(pc.points.toArray)
        assert(polygon.angles.getSet.size == polygon.size)
      }
    }
//    "generatePolygonsWithPoints" in {
//      val gen = ConvexHullGenerator.getConvexHullFromSortedPoints()
//    }
  }

}

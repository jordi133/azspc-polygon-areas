package polygonalareas.generators

import polygonalareas.{AnglesSet, LineSegment, Point}

import scala.util.Random

/**
  * Created by Jordi on 2-1-2017.
  *
  * Generates a polygon by starting with a triangle and then adding new points, while maintaining the invariants
  */
class TriangleBasedGenerator(n: Int, seed: Int = Random.nextInt()) {

  def getStartingTriangle(): Seq[Point] = {
    ???
  }

  def getPolygons(init: IndexedSeq[Point]) = {

    var tc = TriangleConstruct(init)
    while (tc.rest.nonEmpty) {
      val diffs = for {
        i <- tc.points.indices
        p <- tc.rest if tc.canAddPoint(i, p)
      } yield (i, p) -> tc.doubleSurfaceDiffForPoint(i, p)

      val (i, p) = diffs.maxBy(_._2)._1
      tc = tc.addPoint(i, p)
    }
  }

  object TriangleConstruct {
    private val allPoints = ((1 to n) zip (1 to n)).map { case (x, y) => Point(x, y) }

    def apply(points: IndexedSeq[Point]): TriangleConstruct = {
      val rest = allPoints filter (p => points.exists(_.x == p.x) || points.exists(_.y == p.y))
      new TriangleConstruct(points, rest, AnglesSet.fromPoints(points))
    }
  }

  class TriangleConstruct private(val points: IndexedSeq[Point], val rest: Seq[Point], angles: AnglesSet) {
    /**
      * Invariants:
      * - angles.size == points.size (there are no parallel angles)
      * - there are no intersecting edges
      */

    def addPoint(i: Int, p: Point): TriangleConstruct = {
      val newRest = rest filter (point => p.x == point.x || p.y == point.y)
      val newPoints = points.take(i) ++ (p +: points.drop(i))
      val clearedAngles = angles.remove(points(i) - points((i + 1) % points.size))
      val newAngles = clearedAngles.put(points(i) - p).put(points((i + 1) % points.size) - p)
      new TriangleConstruct(newPoints, newRest, newAngles)
    }

    /**
      * Checks whether adding point p at index i breaks invariants
      *
      * @param i
      * @param p
      * @return
      */
    def canAddPoint(i: Int, p: Point): Boolean = {
      //if points(i) - p is parallel with points(i) - points(i+1) then it is also parallel with points(i+1) - p
      def anglesOk: Boolean = !angles.contains(AnglesSet.normalize(points(i) - p)) &&
        !angles.contains(AnglesSet.normalize(points((i + 1) % points.size) - p))
      def intersectionsOk: Boolean = {
        val newEdge1 = LineSegment(p, points(i))
        val newEdge2 = LineSegment(p, points((i + 1) % points.length))
        points.indices.exists { index =>
          val ls = LineSegment(points(index), points((index + 1) % points.size))
          index != i && ((newEdge1 intersects ls) || (newEdge2 intersects ls))
        }
      }
      def pointOk: Boolean = rest.contains(p)

      pointOk && anglesOk && intersectionsOk
    }

    /**
      * Calculates the difference in surface when point p is inserted at index i.
      *
      * Returns positive value for increasing surface iff points are ordered clockwise
      */
    def doubleSurfaceDiffForPoint(i: Int, p: Point) = {
      val v1 = p - points((i + points.size - 1) % points.size)
      val v2 = p - points(i % points.size)
      v1 x v2
    }

    def nextStep(maxAmount: Int): Set[TriangleConstruct] = {
      val diffs: IndexedSeq[((Int, Point), Int)] = for {
        i <- points.indices
        p <- rest if canAddPoint(i, p)
      } yield (i, p) -> doubleSurfaceDiffForPoint(i, p)

      val results = diffs.sortBy(-_._2).map(_._1).take(maxAmount)
      results.map { case (i, p) => addPoint(i, p) }.toSet
    }
  }

}

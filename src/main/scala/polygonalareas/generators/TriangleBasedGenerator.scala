package polygonalareas.generators

import polygonalareas.{AnglesSet, LineSegment, Point}

import scala.util.Random

/**
  * Created by Jordi on 2-1-2017.
  *
  * Generates a polygon by starting with a triangle and then adding new points, while maintaining the invariants
  *
  * Problem: a final polygon might be valid while the same polygon with one point less is invalid
  */
class TriangleBasedGenerator(n: Int, seed: Int = Random.nextInt()) {

  def getStartingTriangle(): Seq[Point] = {
    ???
  }

  def getPolygons(init: IndexedSeq[Point]): Set[IndexedSeq[Point]] = {

    val tc = TriangleConstruct(init)

    var pop = Set(tc)

    while (pop.iterator.hasNext && pop.iterator.next.rest.nonEmpty) {
      pop = pop flatMap (_.nextStep(-1))
      println(s"pop size: ${pop.size}")
    }

    pop map (_.points)

  }

  object TriangleConstruct {
    private val allPoints = for {
      x <- 1 to n
      y <- 1 to n
    } yield Point(x, y)

    def apply(points: IndexedSeq[Point]): TriangleConstruct = {
      require(points.forall{p => 1 <= p.x && p.x <= n && 1 <= p.y && p.y <= n}, s"A point exceeds bounds [1,$n] in $points")
      val rest = allPoints filter (p => !points.exists(_.x == p.x)) filter (p => !points.exists(_.y == p.y))
      println(s"result of init($points): rest=$rest")
      new TriangleConstruct(points, rest, AnglesSet.fromPoints(points))
    }
  }

  case class TriangleConstruct private(points: IndexedSeq[Point], rest: Seq[Point], angles: AnglesSet) {
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
        !points.indices.exists { index =>
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

      val amount = if (maxAmount > 0) maxAmount else diffs.size
      val results = diffs.sortBy(-_._2).map(_._1).take(amount)
      //      println(s"results for nextStep of $this: $results")
      results.map { case (i, p) => addPoint(i, p) }.toSet
    }

    def nextStepWithParallleEdges(maxAmount: Int): Set[TriangleConstruct] = {
      val diffs: IndexedSeq[((Int, Point), Int)] = for {
        i <- points.indices
        p <- rest if canAddPoint(i, p)
      } yield (i, p) -> doubleSurfaceDiffForPoint(i, p)

      val amount = if (maxAmount > 0) maxAmount else diffs.size
      val results = diffs.sortBy(-_._2).map(_._1).take(amount)
      //      println(s"results for nextStep of $this: $results")
      results.map { case (i, p) => addPoint(i, p) }.toSet
    }
  }

}

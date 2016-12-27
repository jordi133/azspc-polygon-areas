package polygonalareas.generators

import polygonalareas.{Point, Vector2D}

import scala.annotation.tailrec
import polygonalareas._

/**
  * Created by Jordi on 25-12-2016.
  */
object ConvexHullGenerator {

  def getConvexHull(points: Set[Point]): (Seq[Point], Set[Point]) = {
    getConvexHullFromSortedPoints(points.toIndexedSeq.sortBy(_.x))
  }

  /**
    * Pre: points is sorted on x coordinate
    *
    * @param points
    * @return a tuple consisting of (the convex hull, the remaining points)
    */
  def getConvexHullFromSortedPoints(points: IndexedSeq[Point]): (IndexedSeq[Point], Set[Point]) = {
    val leftMost = points.head
    val rightMost = points.last

    def belowLine(p: Point): Boolean = rightTurn(leftMost, rightMost, p)
    @tailrec
    def clearConvexHull(convexHull: IndexedSeq[Point], rest:Set[Point]): (IndexedSeq[Point], Set[Point]) = {
      if (convexHull.size > 2 && rightTurn(convexHull.tail.tail.head, convexHull.tail.head, convexHull.head)) {
        clearConvexHull(convexHull.head +: convexHull.tail.tail, rest + convexHull.tail.head)
      } else {
        (convexHull, rest)
      }
    }
    @tailrec
    def buildConvexHull(pointsToProcess: Seq[Point], convexHull: IndexedSeq[Point] = IndexedSeq.empty[Point], rest: Set[Point] = Set.empty[Point]):  (IndexedSeq[Point], Set[Point]) = {
      pointsToProcess match {
        case p +: ps =>
          val (newConvexHull, newRest) = clearConvexHull(p +: convexHull, rest)
          buildConvexHull(ps, newConvexHull, newRest)
        case Nil =>
          (convexHull, rest)
      }
    }

    val (lower, upper) = points.partition(p => belowLine(p))
    val (chPhase1, restPhase1) = buildConvexHull(lower)
    val (convexHull, rest) = buildConvexHull(upper.reverse, chPhase1, restPhase1)

    (convexHull, rest)
  }
}

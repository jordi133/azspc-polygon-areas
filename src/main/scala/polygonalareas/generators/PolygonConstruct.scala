package polygonalareas.generators

import polygonalareas._
import PolygonConstruct._

/**
  * Created by Jordi on 28-12-2016.
  */
object PolygonConstruct {

  /**
    * checks whether a point can be injected in the points polygon without introducing a parallel edge and without placing a point
    * in rest outside of the polygon
    */
  def injectingIsAllowed(points: IndexedSeq[Point], indexToInjectAtEdge: Int, pointToInject: Point, rest: Set[Point]): Boolean = {
    //    val indexToInjectAt = (indexToInjectAtEdge + 1) % points.size
    //    val placementOk = !rest.exists(p => liesInTriangle(p, points(indexToInjectAtEdge), points((indexToInjectAtEdge + 1) % points.length), pointToInject))
    //    val newAngle1 = AnglesSet.normalize(pointToInject - points(indexToInjectAtEdge))
    //    val newAngle2 = AnglesSet.normalize(pointToInject - points((indexToInjectAtEdge + 1) % points.length))
    //    val angleOk = points.indices.forall { i =>
    //      val angle = AnglesSet.normalize(points((i + 1) % points.length) - points(i))
    //      i + 1 != indexToInjectAt && angle != newAngle1 && angle != newAngle2
    //    }
    //    placementOk && angleOk
    injectingLeavesRestInside(points, indexToInjectAtEdge, pointToInject, rest) &&
      injectingCreatesParallelEdges(points, indexToInjectAtEdge, pointToInject, rest).isEmpty &&
      !injectingCreatesIntersectingEdges(points, indexToInjectAtEdge, pointToInject, rest)
  }

  def injectingLeavesRestInside(points: IndexedSeq[Point], indexToInjectAtEdge: Int, pointToInject: Point, rest: Set[Point]): Boolean = {
    !rest.exists(p => liesInTriangle(p, points(indexToInjectAtEdge), points((indexToInjectAtEdge + 1) % points.length), pointToInject))
  }

  def injectingCreatesParallelEdges(points: IndexedSeq[Point], indexToInjectAtEdge: Int, pointToInject: Point, rest: Set[Point]): Seq[Seq[Int]] = {
    val indexToInjectAt = (indexToInjectAtEdge + 1) % points.size
    val newAngle1 = AnglesSet.normalize(pointToInject - points(indexToInjectAtEdge))
    val newAngle2 = AnglesSet.normalize(pointToInject - points((indexToInjectAtEdge + 1) % points.length))
    var edgesParallelToAngle1 = Seq.empty[Int]
    var edgesParallelToAngle2 = Seq.empty[Int]
    for (i <- points.indices) {
      val angle = AnglesSet.normalize(points((i + 1) % points.length) - points(i))
      if (i + 1 != indexToInjectAt && angle == newAngle1) edgesParallelToAngle1 +:= i
      if (i + 1 != indexToInjectAt && angle == newAngle2) edgesParallelToAngle2 +:= i
    }
    val newAnglesParallel = if (newAngle1 == newAngle2) Seq(indexToInjectAtEdge, indexToInjectAtEdge + 1) else Seq.empty[Int]
    (newAnglesParallel +: edgesParallelToAngle1 +: Seq(edgesParallelToAngle2)).filter(_.nonEmpty)
  }

  def injectingCreatesIntersectingEdges(points: IndexedSeq[Point], indexToInjectAtEdge: Int, pointToInject: Point, rest: Set[Point]): Boolean = {
    val newEdge1 = LineSegment(pointToInject, points(indexToInjectAtEdge))
    val newEdge2 = LineSegment(pointToInject, points((indexToInjectAtEdge + 1) % points.length))
    points.indices.exists { i =>
      val ls = LineSegment(points(i), points((i + 1) % points.length))
      i != indexToInjectAtEdge && ((newEdge1 intersects ls) || (newEdge2 intersects ls))
    }
  }

  /**
    * p1 and p2 are part of a polygon. Pre condition: 'point' and 'p3' lie on the same side of p1p2
    */
  def liesInTriangle(point: Point, p1: Point, p2: Point, p3: Point) = {
    val r1 = rightTurn(p1, p2, point)
    val r2 = rightTurn(p2, p3, point)
    val r3 = rightTurn(p3, p1, point)
    r1 == r2 && r2 == r3
  }
}

/**
  * @param points        a polygon
  * @param rest          a set of points that lie within the polygon defined by points
  * @param parallelEdges all parallel edges (grouped) indicated by their starting vertex
  */
case class PolygonConstruct(points: IndexedSeq[Point], rest: Set[Point], parallelEdges: Seq[Seq[Int]]) {
  val parallelEdgesCount = parallelEdges.map(edges => edges.length - 1).sum

  def parallelEdgesCountAfterMovingPoint(index: Int): Int = {
    val removed = parallelEdges.count(seq => seq.contains(index) && seq.contains((index + 1) % points.length))
    parallelEdgesCount - removed
  }

  def nextStep: Set[PolygonConstruct] = {
    if (parallelEdgesCount <= rest.size) {
      nextStepNoParallelEdges
    } else {
      Set.empty[PolygonConstruct]
    }
  }

  def nextStepNoParallelEdges: Set[PolygonConstruct] = {
    def findNewParallelEdges(points: IndexedSeq[Point], pointAdded: Int): Seq[Seq[Int]] = {
      val newAngle1Index = (pointAdded + points.size - 1) % points.size
      val newAngle2Index = (pointAdded + points.size) % points.size
      val newAngle1 = AnglesSet.normalize(points(pointAdded) - points(newAngle1Index))
      val newAngle2 = AnglesSet.normalize(points(pointAdded) - points(newAngle2Index))

      for {
        i <- points.indices if i - 1 != pointAdded && i != pointAdded
        vector = points((i + 1) % points.length) - points(i)
        norm = AnglesSet.normalize(vector) if norm == newAngle1 || norm == newAngle2
      } yield {
        if (norm == newAngle1) Seq(newAngle1Index, i)
        else Seq(newAngle2Index, i)
      }
    }
    def potentialPointsFilter(pointToInject: Point, indexToInjectAfter: Int): Boolean = {
      val restInsize = injectingLeavesRestInside(points, indexToInjectAfter, pointToInject, rest - pointToInject)
      val intersections = !injectingCreatesIntersectingEdges(points, indexToInjectAfter, pointToInject, rest)
      val createsParallelEdgesOk = parallelEdgesCountAfterMovingPoint(indexToInjectAfter) + injectingCreatesParallelEdges(points, indexToInjectAfter, pointToInject, rest - pointToInject).size < rest.size
      restInsize && intersections && createsParallelEdgesOk
    }
    // don't create more new parallel edges than there will be points left in rest
    val possibleIndices = points.indices
    val result = possibleIndices.toSet.map { indexToInjectAfter: Int =>
      val potentialPoints = rest.filter { p => potentialPointsFilter(p, indexToInjectAfter) }
      potentialPoints.map { p =>
        val newPoints = points.take(indexToInjectAfter + 1) ++ (p +: points.drop(indexToInjectAfter + 1))
        val newParallelEdges = findNewParallelEdges(newPoints, indexToInjectAfter + 1)
        PolygonConstruct(newPoints, rest - p, newParallelEdges)
      }
    }
    //    result.flatten.filter(pc => pc.parallelEdges.size <= pc.rest.size)
    if (result.flatten.isEmpty) Set.empty
    else result.flatten
  }


  def nextStepRemoveParallelEdge: Set[PolygonConstruct] = {
    val possibleIndices = parallelEdges.head

    val result = possibleIndices.toSet.map { indexToInjectAfter: Int =>
      val potentialPoints = rest.filter { p => injectingIsAllowed(points, indexToInjectAfter, p, rest - p) }
      potentialPoints.map { p =>
        val newPoints = points.take(indexToInjectAfter + 1) ++ (p +: points.drop(indexToInjectAfter + 1))
        val newParallelEdges =
          if (parallelEdges.head.length > 2) {
            parallelEdges.head.filter(_ != indexToInjectAfter) +: parallelEdges.tail
          } else {
            parallelEdges.tail
          }
        PolygonConstruct(newPoints, rest - p, newParallelEdges)
      }
    }

    result.flatten
  }
}

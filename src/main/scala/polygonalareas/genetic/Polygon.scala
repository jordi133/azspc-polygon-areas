package polygonalareas.genetic

import polygonalareas.{AnglesSet, LineSegment, Point, Vector2D, doubleSurface}

import scala.util.Random

/**
  * Created by Jordi on 4-2-2017.
  */

case class Polygon(points: IndexedSeq[Point]) {
  lazy val parEdges = angles.values.filter(_.size > 1).flatten.size
  lazy val isValid = angles.size == points.size
  lazy val score = doubleSurface(points)
  lazy val edges: Seq[LineSegment] = (points zip (points.tail :+ points.head)).map{case (p1, p2) => LineSegment(p1,p2)}.toSeq
  lazy val isSelfIntersecting: Boolean = edges.exists(ls1 => edges.exists(ls2 => ls1 != ls2 && (ls1 intersects ls2)))

  lazy val angles: Map[Vector2D, Seq[Int]] =  {
    def calcMap(i: Int, acc: Map[Vector2D, Seq[Int]]): Map[Vector2D, Seq[Int]] = {
      if (i < points.size) {
        val angle = AnglesSet.normalize(points((i + 1) % points.size) - points(i))
        calcMap(i + 1, acc.updated(angle, i +: acc.getOrElse(angle, Seq.empty)))
      } else {
        acc
      }
    }
    calcMap(0, Map.empty)
  }

  def nextGen(maxOffSpring: Int)(implicit random: Random): Seq[Polygon] = {
    import Mutater._
    val n: Int = points.length

    // mutate all parallel edges if they exist, otherwise pick random points to mutate
    val indicesOfParallelEdges = angles.values.filter(_.length > 1).flatMap(_.sorted).toSeq
    val indicesToHandle: Seq[Seq[Int]] = {
      if (indicesOfParallelEdges.isEmpty) {
        // if no parallel edges exist, then find pair of points to mutate
        for (i <- 1 to maxOffSpring) yield if (i % 2 == 0) randomPairOfIndices(n) else randomPairOfNeighboringIndices(n)
      } else {
        // else use parallel edges to mutate
        random.shuffle((indicesOfParallelEdges flatMap { index =>
          getIndicesToMutate(n, maxOffSpring, index)
        }).distinct).take(maxOffSpring)
      }
    }

    val newPolygonsFromReordering = for {
      indices <- indicesToHandle
      indicesToSwap = (indices.head, indices(1))
      (newPolygon, indicesThatChange) = Mutater.reorder(points, indicesToSwap._1, indicesToSwap._2)
      polygon = Polygon(newPolygon) if !polygon.hasSelfIntersectingEdgesOnIndices(indicesThatChange)
    } yield polygon

    val newPolygonsFromPointMutations = for {
      indices <- indicesToHandle
      newPolygon <- Mutater.mutateGivenIndices(points, indices)
      polygon = Polygon(newPolygon.toIndexedSeq) if !polygon.hasSelfIntersectingEdgesOnIndices(indices)
    } yield polygon

    newPolygonsFromReordering ++ newPolygonsFromPointMutations
  }

  def hasSelfIntersectingEdgesOnIndices(indices: Seq[Int]): Boolean = {
    def getLineSegment(i: Int) = LineSegment(points(i), points((i + 1) % points.size))
    val lineSegments = points.indices.map(getLineSegment)
    def isNonIntersecting(ls: LineSegment): Boolean = lineSegments.forall(lineSegment => !(lineSegment intersects ls) || lineSegment.contains(ls.p1) || lineSegment.contains(ls.p2))

    val indicesToCheck = indices.flatMap(i => Seq((i + points.length - 1) % points.length, i)).distinct
    !indicesToCheck.forall { i => isNonIntersecting(getLineSegment(i)) }
  }
}

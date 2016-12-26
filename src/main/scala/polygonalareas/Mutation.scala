package polygonalareas

import scala.util.Random
import Implicits.LineSegmentOps
import Implicits.PointToTuple

/**
  * Created by Jordi on 14-12-2016.
  *
  * TODO: Structurally generate mutation, prevent generating the same twice
  */

sealed trait MutationAttempt {
  def p: Polygon

  def isDefined = !isEmpty

  def isEmpty = toOption.isEmpty

  def toOption: Option[Polygon] = this match {
    case Success(p) => Some(p)
    case _ => None
  }
}

case class Success(p: Polygon) extends MutationAttempt

sealed trait MutationFailure extends MutationAttempt

case class OriginalGenerated(p: Polygon) extends MutationAttempt

case class DuplicateAngle(p: Polygon) extends MutationFailure

case class SelfIntersection(p: Polygon, i1: Int, i2: Int, i3: Int, i4: Int) extends MutationFailure

object Mutation {

  def tryMutation(p: Polygon): Option[Polygon] = tryMutation(p, 2).toOption

  def mutate(p: Polygon, nrOfPoints: Int = 2): Polygon = {
    var attempts = 0
    val maxAttempts = 100
    var done = false
    var result = tryMutation(p, nrOfPoints)
    while (result.isEmpty) {
      result = tryMutation(p, nrOfPoints)

      if (result.isEmpty) {
        attempts += 1
        if (attempts == maxAttempts) throw new RuntimeException(s"Cannot find valid mutation on $p")
      } else {
        done = true
      }
    }

    result.p
  }

  def tryMutation(p: Polygon, nrOfPoints: Int): MutationAttempt = {
    val indicesToChange = Random.shuffle(p.points.indices.toList).take(nrOfPoints).toIndexedSeq
    val pointsToChange = indicesToChange map (i => p.points(i))
    val (xCoords, yCoords) = pointsToChange.unzip

    val changedPoints = for ((x, y) <- Random.shuffle(xCoords) zip Random.shuffle(yCoords)) yield Point(x, y)

    if (pointsToChange.toList == changedPoints) {
      OriginalGenerated(p)
    } else {
      val newPoints = new Array[Point](p.size)
      p.points.copyToArray(newPoints)
      for ((i, p) <- indicesToChange zip changedPoints) newPoints.update(i, p)

      val result = Polygon(newPoints)
      val testResult =
        if (result.angles.getSet.size != result.size) Some(DuplicateAngle(p))
        else testMutationOnSelfIntersection(p, indicesToChange, changedPoints, result)

      if (testResult.isEmpty) {
        println(s"Resulting polynom: $result from changing $pointsToChange to $changedPoints")
      }
      if (testResult.isEmpty) Success(result) else testResult.get
    }
  }

  def testMutationOnSelfIntersection(p: Polygon, removedPoints: IndexedSeq[Int], addedPoints: Seq[Point], newPolygon: Polygon): Option[MutationFailure] = {
    // determine edges to remove from polygon
    val edgesToRemove = new Array[LineSegment](removedPoints.length * 2)
    for (i <- removedPoints.indices) {
      val remP = removedPoints(i)
      val ls1 = LineSegment(p.getPointModulo(remP - 1), p.getPointModulo(remP))
      val ls2 = LineSegment(p.getPointModulo(remP), p.getPointModulo(remP + 1))
      edgesToRemove.update(2 * i, ls1)
      edgesToRemove.update(2 * i + 1, ls2)
    }

    // remove edges
    var clearedEdges: Seq[LineSegment] = p.edges diff edgesToRemove

    // determine new edges to add
    val edgesToAdd = new Array[LineSegment](removedPoints.length * 2)

    for (i <- removedPoints.indices) {
      val newP = addedPoints(i)
      val remP = removedPoints(i)
      val ls1 = LineSegment(newPolygon.getPointModulo(remP - 1), newP)
      val ls2 = LineSegment(newP, newPolygon.getPointModulo(remP + 1))
      edgesToAdd.update(2 * i, ls1)
      edgesToAdd.update(2 * i + 1, ls2)
    }

    // add new angles one by one and verify that they are new to the polygon
    var testResult = Option.empty[MutationFailure]
    var i = 0
    while (testResult.isEmpty && i < edgesToAdd.length) {
      clearedEdges.find(ls => ls intersects edgesToAdd(i)) match {
        case None =>
          clearedEdges = edgesToAdd(i) +: clearedEdges
          i += 1
        case Some(edge) =>
          val i1 = newPolygon.points.indexOf(edge.p1)
          val i2 = newPolygon.points.indexOf(edge.p2)
          val i3 = newPolygon.points.indexOf(edgesToAdd(i).p1)
          val i4 = newPolygon.points.indexOf(edgesToAdd(i).p2)
          testResult = Some(SelfIntersection(newPolygon, i1, i2, i3, i4))
      }
    }

    testResult
  }

}

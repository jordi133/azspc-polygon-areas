package polygonalareas

import scala.util.Random
import Implicits.LineSegmentOps

/**
  * Created by Jordi on 14-12-2016.
  *
  * TODO: Structurally generate mutation, prevent generating the same twice
  */

sealed trait MutationAttempt {
  def p: Polygon

  def isEmpty = this match {
    case Success(_) => false
    case _ => true
  }

  def isDefined = !isEmpty

  def toOption: Option[Polygon] = this match {
    case Success(p) => Some(p)
    case _ => None
  }
}

case class Success(p: Polygon) extends MutationAttempt

sealed trait MutationFailure extends MutationAttempt

case class DuplicateAngle(p: Polygon, i1: Int, i2: Int, i3: Int, i4: Int) extends MutationFailure

case class SelfIntersection(p: Polygon, i1: Int, i2: Int, i3: Int, i4: Int) extends MutationFailure

// TODO remove class
case class BasicMutationFailure(p: Polygon) extends MutationFailure

//case class MultipleDuplicateAngle(p: Polygon, indices: List[(Int, Int, Int, Int)]) extends MutationAttempt(p)

//case class MultipleSelfIntersections(p: Polygon, indices: List[(Int, Int, Int, Int)]) extends MutationAttempt(p)

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
    //  def tryMutation(p: Polygon, nrOfPoints: Int): Option[Polygon] = {
    val indicesToChange = Random.shuffle(p.points.indices.toList).take(nrOfPoints).toIndexedSeq
    val pointsToChange = indicesToChange map (i => p.points(i))

    val changedPoints =
      for ((xi, yi) <- pointsToChange.indices.toList zip Random.shuffle(pointsToChange.indices.toList))
        yield (pointsToChange(xi)._1, pointsToChange(yi)._2)

    // TODO if this change leads to the original polygon, then shuffle changedPoints

    val newPoints = new Array[Point](p.size)
    p.points.copyToArray(newPoints)
    for ((i, p) <- indicesToChange zip changedPoints) newPoints.update(i, p)

    val result = Polygon(newPoints)

    val testResult = testMutation(p, indicesToChange, changedPoints, result)

    if (testResult.isEmpty) println(s"Resulting polynom: $result from changing $pointsToChange to $changedPoints")
    if (testResult.isEmpty) Success(result) else testResult.get
  }

  def testMutation(p: Polygon, removedPoints: IndexedSeq[Int], addedPoints: Seq[Point], newPolygon: Polygon): Option[MutationFailure] = {
    // determine angles to remove from polygon
    val anglesToRemove = new Array[Vector2D](removedPoints.length * 2)
    // determine edges to remove from polygon
    val edgesToRemove = new Array[Vector2D](removedPoints.length * 2)
    for (i <- removedPoints.indices) {
      val remP = removedPoints(i)
      val ls1 = (p.getPointModulo(remP - 1), p.getPointModulo(remP))
      val ls2 = (p.getPointModulo(remP), p.getPointModulo(remP + 1))
      anglesToRemove.update(2 * i, ls1.vector)
      anglesToRemove.update(2 * i + 1, ls2.vector)
      edgesToRemove.update(2 * i, ls1.vector)
      edgesToRemove.update(2 * i + 1, ls2.vector)
    }

    // remove angles
    var clearedAngles = p.angles.removeAll(anglesToRemove.toSet)
    // remove edges
    var clearedEdges: Seq[LineSegment] = p.edges diff edgesToRemove

    // determine new angles to add
    val anglesToAddArray = new Array[Vector2D](removedPoints.length * 2)
    // determine new edges to add
    val edgesToAdd = new Array[LineSegment](removedPoints.length * 2)

    for (i <- removedPoints.indices) {
      val newP = addedPoints(i)
      val remP = removedPoints(i)
      val ls1 = (newPolygon.getPointModulo(remP - 1), newP)
      val ls2 = (newP, newPolygon.getPointModulo(remP + 1))
      anglesToAddArray.update(2 * i, ls1.vector)
      anglesToAddArray.update(2 * i + 1, ls2.vector)
      edgesToAdd.update(2 * i, ls1)
      edgesToAdd.update(2 * i + 1, ls2)
    }

    // Put angles in anglesSet to prevent the same angle from being added twice (by having adjacent points changed)
    var anglesToAddSet = AnglesSet.empty
    for (a <- anglesToAddArray) anglesToAddSet = anglesToAddSet.put(a)

    // add new angles one by one and verify that they are new to the polygon
    var result = Option.empty[MutationFailure]
    val anglesToAddSeq = anglesToAddSet.getSet.toIndexedSeq
    var i = 0
    while (result.isEmpty && i < anglesToAddSeq.size) {
      if (!clearedAngles.contains(anglesToAddSeq(i))) {
        clearedAngles = clearedAngles.put(anglesToAddSeq(i))
        i += 1
      } else {
        result = Some(BasicMutationFailure(newPolygon))
      }
    }
    i = 0
    while (result.isEmpty && i < edgesToAdd.length) {
      clearedEdges.find(ls => ls intersects edgesToAdd(i)) match {
        case None =>
          clearedEdges = edgesToAdd(i) +: clearedEdges
          i += 1
        case Some(edge) =>
          val edge = clearedEdges.find(ls => ls intersects edgesToAdd(i)).get
          val i1 = newPolygon.points.indexOf(edge._1)
          val i2 = newPolygon.points.indexOf(edge._2)
          val i3 = newPolygon.points.indexOf(edgesToAdd(i)._1)
          val i4 = newPolygon.points.indexOf(edgesToAdd(i)._2)

          result = Some(SelfIntersection(newPolygon, i1, i2, i3, i4))
      }
    }

    result
  }

}

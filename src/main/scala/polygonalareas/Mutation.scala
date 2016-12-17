package polygonalareas

import scala.util.Random
import Implicits.LineSegmentOps

/**
  * Created by Jordi on 14-12-2016.
  *
  * TODO: Structurally generate mutation, prevent generating the same twice
  */
object Mutation {

  def tryMutation(p: Polygon): Option[Polygon] = tryMutation(p, 2)

  def mutate(p: Polygon): Polygon = {
    var attempts = 0
    val maxAttempts = 100
    var done = false
    var result = tryMutation(p, 2)
    while (result.isEmpty) {
      result = tryMutation(p, 2)

      if (result.isEmpty) {
        attempts += 1
        if (attempts == maxAttempts) throw new RuntimeException(s"Cannot find valid mutation on $p")
      } else {
        done = true
      }
    }

    result.get
  }

  def tryMutation(p: Polygon, nrOfPoints: Int): Option[Polygon] = {
    val indicesToChange = Random.shuffle(p.points.indices.toList).take(nrOfPoints).toIndexedSeq
    val pointsToChange = indicesToChange map (i => p.points(i))

    val changedPoints =
      for ((xi, yi) <- pointsToChange.indices.toList zip Random.shuffle(pointsToChange.indices.toList))
        yield (pointsToChange(xi)._1, pointsToChange(yi)._2)

    val originalPoints = indicesToChange.map(p.points(_))

    val newPoints = new Array[Point](p.size)
    p.points.copyToArray(newPoints)
    for ((i, p) <- indicesToChange zip changedPoints) newPoints.update(i, p)
    val result = Polygon(newPoints)

    val valid = testMutation(p, indicesToChange, changedPoints, result)

    if (valid) println(s"Resulting polynom: $result from changing $pointsToChange to $changedPoints")
    if (valid) Some(result) else None
  }

  def testMutation(p: Polygon, removedPoints: IndexedSeq[Int], addedPoints: Seq[Point], newPolygon: Polygon): Boolean = {
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

    // Put angles in anglesSet to prevent the same angle from being added twice (by having adjacent points changes)
    var anglesToAddSet = AnglesSet.empty
    for (a <- anglesToAddArray) anglesToAddSet = anglesToAddSet.put(a)

    // add new angles one by one and verify that they are new to the polygon
    var valid = true
    val anglesToAddSeq = anglesToAddSet.getSet.toIndexedSeq
    var i = 0
    while (valid && i < anglesToAddSeq.size) {
      if (!clearedAngles.contains(anglesToAddSeq(i))) {
        clearedAngles = clearedAngles.put(anglesToAddSeq(i))
        i += 1
      } else {
        //        println(s"Double angle: ${anglesToAddSeq(i)} in $clearedAngles")
        valid = false
      }
    }
    i = 0
    while (valid && i < edgesToAdd.length) {
      if (!clearedEdges.exists(ls => ls intersects edgesToAdd(i))) {
        clearedEdges = edgesToAdd(i) +: clearedEdges
        i += 1
      } else {
        val edge = clearedEdges.find(ls => ls intersects edgesToAdd(i)).get
        //        println(s"Intersecting edges: $edge and ${edgesToAdd(i)}: ${edge intersects edgesToAdd(i)}")
        valid = false
      }
    }

    valid
  }

}

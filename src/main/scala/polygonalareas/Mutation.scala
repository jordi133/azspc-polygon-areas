package polygonalareas

import scala.util.Random
import Implicits.LineSegmentOps
/**
  * Created by Jordi on 14-12-2016.
  */
trait Mutation {
  def mutate(p: Polygon): Polygon
}

object CoordinateSwapMutation extends Mutation {
  override def mutate(p: Polygon): Polygon = {
    val newPoints = new Array[Point](p.size)
    p.points.copyToArray(newPoints)
    val result = Polygon(newPoints)

    var attempts = 0
    val maxAttempts = 100
    var done = false
    while (!done) {
      p.points.copyToArray(newPoints)
      val i1 = Random.nextInt(p.size)
      val p1 = p.points(i1)
      val i2a = Random.nextInt(p.size - 1)
      val i2 = if (i2a == i1) p.size - 1 else i2a
      val p2 = p.points(i2)

      newPoints(i1) = (p1._1, p2._2)
      newPoints(i2) = (p2._1, p1._2)

      val removedPoints = Seq(i1, i2)
      val addedPoints = Seq(p1, p2)
      val valid = testMutationOnAngles(result, removedPoints, addedPoints) &&
        testMutationOnSelfIntersecting(result, removedPoints, addedPoints)

      if (!valid) {
        attempts += 1
        if (attempts == maxAttempts) throw new RuntimeException(s"Cannot find valid mutation on $p")
      } else {
        done = true
      }
    }

    println(s"result ${result.points.mkString(", ")} after $attempts attempts")

    result
  }

  // TODO merge two test functions below

  /**
    * @param p A valid polygon, meaning no two edges of it are parallel
    * @param removedPoints indices of the points that will be removed
    * @param newPoints the new points, in the order in which they will be placed
    * @return true iff no two egdes of the new polygon are parallel
    */
  def testMutationOnAngles(p: Polygon, removedPoints: Seq[Int], newPoints: Seq[Point]): Boolean = {
    // determine angles to remove from polygon
    val anglesToRemove = new Array[Vector2D](removedPoints.length * 2)
    for (i <- removedPoints.indices) {
      val remP = removedPoints(i)
      val ls1 = (p.getPointModulo(remP - 1), p.getPointModulo(remP))
      val ls2 = (p.getPointModulo(remP), p.getPointModulo(remP + 1))
      anglesToRemove.update(2 * i, ls1.vector)
      anglesToRemove.update(2 * i + 1, ls2.vector)
    }

    // remove angles
    var clearedAngles = p.angles.removeAll(anglesToRemove.toSet)

    // determine new angles to add
    val anglesToAddArray = new Array[Vector2D](removedPoints.length * 2)
    for (i <- removedPoints.indices) {
      val newP = newPoints(i)
      val remP = removedPoints(i)
      val ls1 = (p.getPointModulo(remP - 1), newP)
      val ls2 = (newP, p.getPointModulo(remP + 1))
      anglesToAddArray.update(2 * i, ls1.vector)
      anglesToAddArray.update(2 * i + 1, ls2.vector)
    }

    // Put angles in anglesSet to prevent the same angle from being added twice (by having adjacent points changes)
    val anglesToAddSet = AnglesSet.empty
    for (a <- anglesToAddArray) anglesToAddSet.put(a)

    // add new angles one by one and verify that they are new to the polygon
    var valid = true
    val anglesToAddSeq = anglesToAddSet.getSet.toIndexedSeq
    var i = 0
    while (valid && i < anglesToAddSeq.size) {
      if (!clearedAngles.contains(anglesToAddSeq(i))) {
        clearedAngles = clearedAngles.put(anglesToAddSeq(i))
        i += 1
      } else {
        valid = false
      }
    }

    valid
  }

  /**
    * @param p a valid polygon, ie one that is not self intersecting
    * @param removedPoints
    * @param newPoints
    * @return true iff the new polygon is not self intersecting
    */
  def testMutationOnSelfIntersecting(p: Polygon, removedPoints: Seq[Int], newPoints: Seq[Point]): Boolean = {
    // determine edges to remove from polygon
    val edgesToRemove = new Array[Vector2D](removedPoints.length * 2)
    for (i <- removedPoints.indices) {
      val remP = removedPoints(i)
      val ls1 = (p.getPointModulo(remP - 1), p.getPointModulo(remP))
      val ls2 = (p.getPointModulo(remP), p.getPointModulo(remP + 1))
      edgesToRemove.update(2 * i, ls1.vector)
      edgesToRemove.update(2 * i + 1, ls2.vector)
    }

    // remove edges
    var clearedEdges: Seq[LineSegment] = p.edges intersect edgesToRemove

    // determine new edges to add
    val edgesToAdd = new Array[LineSegment](removedPoints.length * 2)
    for (i <- removedPoints.indices) {
      val newP = newPoints(i)
      val remP = removedPoints(i)
      val ls1 = (p.getPointModulo(remP - 1), newP)
      val ls2 = (newP, p.getPointModulo(remP + 1))
      edgesToAdd.update(2 * i, ls1)
      edgesToAdd.update(2 * i + 1, ls2)
    }

    // add new edges one by one and verify that they are not intersecting an existing one
    var valid = true
    var i = 0
    while (valid && i < edgesToAdd.length) {
      if (!clearedEdges.exists(ls => ls intersects edgesToAdd(i))) {
        clearedEdges = edgesToAdd(i) +: clearedEdges
        i += 1
      } else {
        valid = false
      }
    }

    valid
  }

}

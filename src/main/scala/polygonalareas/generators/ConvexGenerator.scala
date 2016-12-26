package polygonalareas.generators

import polygonalareas.{Vector2D, Point}

/**
  * Created by Jordi on 25-12-2016.
  */
object ConvexGenerator {

  def getConvexHull(points: Set[Point]): Seq[Point] = {
    getConvexHullFromSortedPoints(points.toSeq.sortBy(_.x))
  }

  /**
    * Pre: points is sorted on x coordinate
    *
    * @param points
    * @return
    */
  def getConvexHullFromSortedPoints(points: Seq[Point]): Seq[Point] = {
    def leftTurn(p1: Point, p2: Point, p3: Point): Boolean = {
      val ls1: Vector2D = p2 - p1
      val ls2: Vector2D = p3 - p2
      val v = ls1 x ls2
      ???
    }

    val (initialP +: rest) = points


    var lower = Seq(initialP)
    var upper = Seq.empty[Point]

    for (p <- rest) {
      if (p.x <= initialP.x) {
        lower = p +: lower

        // verify angle p, lower.head, lower.tail.head
        if (lower.size > 2 && leftTurn(p, lower.head, lower.tail.head)) {

        }

      } else {
        upper = p +: upper
      }
    }

    ???
  }
//
//  def generateLinearPolygon(points: Seq[Point]): Polygon = {
//    val points = generateRandomPoints
//    val l = points.minBy(_.x)
//    val r = points.maxBy(_.x)
//
//    var leftPoints = Set.empty[Point]
//    var rightPoints = Set.empty[Point]
//    for (p <- points - l - r) {
//      val sign = Math.signum((r.x - l.x) * (p.y - l.y) - (r.y - l.y) * (p.x - l.x))
//      if (sign <= 0) leftPoints += p
//      else rightPoints += p
//    }
//
//    val leftPointsSorted = leftPoints.toIndexedSeq.sortBy(_.x)
//    val rightPointsSorted = rightPoints.toIndexedSeq.sortBy(_.x)
//
//    val pointsArray = new Array[Point](n)
//    pointsArray.update(0, l)
//    pointsArray.update(leftPointsSorted.size + 1, r)
//    for (i <- leftPointsSorted.indices)
//      pointsArray.update(1 + i, leftPointsSorted(i))
//    for (i <- rightPointsSorted.indices)
//      pointsArray.update(n - i - 1, rightPointsSorted(i))
//
//    Polygon(pointsArray)
//  }
}

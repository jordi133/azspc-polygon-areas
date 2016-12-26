package polygonalareas.generators

import polygonalareas.{Vector2D, Point}

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
  def getConvexHullFromSortedPoints(points: IndexedSeq[Point]): (Seq[Point], Set[Point]) = {
    val leftMost = points.head
    val rightMost = points.last

    def rightTurn(p1: Point, p2: Point, p3: Point): Boolean = {
      val ls1: Vector2D = p2 - p1
      val ls2: Vector2D = p3 - p2
      (ls1 x ls2) >= 0
    }
    def belowLine(p: Point): Boolean = rightTurn(leftMost, rightMost, p)

    var convexHull = Seq.empty[Point]
    var rest: Set[Point] = Set.empty[Point]
    var upper = Seq(rightMost)

    var i = 0
    while (i < points.length) {
      if (belowLine(points(i))) {
        // verify angle p, convexHull.head, convexHull.tail.head
        convexHull = points(i) +: convexHull
        while (convexHull.size > 2 && rightTurn(convexHull.head, convexHull.tail.head, convexHull.tail.head)) {
          rest = rest + convexHull.tail.head
          convexHull = convexHull.head +: convexHull.tail.tail
        }
      } else {
        upper = points(i) +: upper
      }
      i += 1
    }

    i = 0
    while (i < upper.length) {
      convexHull = upper(i) +: convexHull
      while (convexHull.size > 2 && rightTurn(convexHull.head, convexHull.tail.head, convexHull.tail.head)) {
        rest = rest + convexHull.tail.head
        convexHull = convexHull.head +: convexHull.tail
      }
      i += 1
    }

    (convexHull, rest)
  }
}

package polygonalareas.genetic

import polygonalareas.{Point, Vector2D}

import scala.util.Random

/**
  * Created by Jordi on 29-1-2017.
  */
object PolygonGenerator {
  def generatePolygonInSquare(pointGenerator: Int => () => IndexedSeq[Point], polygonGenerator: Set[Point] => IndexedSeq[Point])(n: Int): () => IndexedSeq[Point] = { () =>
    val innerPoints = Point(1, 2) +: Point(2, 1) +: (pointGenerator(n - 6)() map (_ + Vector2D(2, 2)))
    val p1 = Point(2, 2)
    val p2 = Point(1, n - 1)
    val p3 = Point(n, n)
    val p4 = Point(n - 1, 1)

    val innerPolygon = polygonGenerator(innerPoints.toSet).map(p => p + Vector2D(2, 2)) // TODO: This makes assumptions on the order of the points here (head being (2,1) and last (1,2))
    if (innerPolygon.contains(p1)) println(s"inner contains p1")
    if (innerPolygon.contains(p2)) println(s"inner contains p2")
    if (innerPolygon.contains(p3)) println(s"inner contains p3")
    if (innerPolygon.contains(p4)) println(s"inner contains p4")
    val result = innerPolygon.head +: p1 +: p2 +: p3 +: p4 +: innerPolygon.tail
    result
  }

  def generatePolygonInSquare(polygonGenerator: Int => IndexedSeq[Point]): (Int) => IndexedSeq[Point] = { (n) =>
    val innerPoints = Point(1, 2) +: Point(2, 1) +: (polygonGenerator(n - 6) map (_ + Vector2D(2, 2)))
    val p1 = Point(2, 2)
    val p2 = Point(1, n - 1)
    val p3 = Point(n, n)
    val p4 = Point(n - 1, 1)

    val innerPolygon = innerPoints.map(p => p + Vector2D(2, 2)) // TODO: This makes assumptions on the order of the points here (head being (2,1) and last (1,2))
    if (innerPolygon.contains(p1)) println(s"inner contains p1")
    if (innerPolygon.contains(p2)) println(s"inner contains p2")
    if (innerPolygon.contains(p3)) println(s"inner contains p3")
    if (innerPolygon.contains(p4)) println(s"inner contains p4")
    val result = innerPolygon.head +: p1 +: p2 +: p3 +: p4 +: innerPolygon.tail
    require(!Polygon(result).isSelfIntersecting, s"Generated polygon is self intersecting: $result")
    result
  }

  def generateDiagonalPolygon(points: Set[Point]): IndexedSeq[Point] = {
    val l = points.minBy(_.x)
    val r = points.maxBy(_.x)
    var leftPoints = Set.empty[Point]
    var rightPoints = Set.empty[Point]
    for (p <- points - l - r) {
      val sign = Math.signum((r.x - l.x) * (p.y - l.y) - (r.y - l.y) * (p.x - l.x))
      if (sign <= 0) leftPoints += p
      else rightPoints += p
    }

    val leftPointsSorted = leftPoints.toIndexedSeq.sortBy(p => p.x + p.y)
    val rightPointsSorted = rightPoints.toIndexedSeq.sortBy(p => -(p.x + p.y))

    (l +: leftPointsSorted) ++ (r +: rightPointsSorted)
  }

  def generateWedgePolygon(pointGenerator: Int => Set[Point]): Int => IndexedSeq[Point] = { n =>
    val r = Point(n,n)
    val p1 = Point(1,2)
    val p2 = Point(2,1)
    val points = pointGenerator(n-3).map(p => p + Vector2D(2,2)) + r + p1 + p2
    var bottomPoints = Set.empty[Point]
    var topPoints = Set.empty[Point]
    for (p <- points) {
      val sign = Math.signum((n - 1) * (p.y - 1) - (n - 1) * (p.x - 1))
      if (sign < 0) bottomPoints += p
      else topPoints += p
    }

    val leftPointsSorted = bottomPoints.toIndexedSeq.sortBy(p => p.y)
    val rightPointsSorted = topPoints.toIndexedSeq.sortBy(p => -p.x)

    val result = leftPointsSorted ++ rightPointsSorted

    if (result.map(_.x).distinct.size != n || result.map(_.y).distinct.size != n) println(s"duplicate coordinates used")
    require(!Polygon(result).isSelfIntersecting, s"Generated polygon is self intersecting: $result")
    result
  }

  def generateDoubleWedgePolygon(pointGenerator: Int => Set[Point]): Int => IndexedSeq[Point] = { n =>
    val points = pointGenerator(n - 2)
    val start = Point(2,1)
    val end = Point(1,2)

    // take all points below (x+y==n)
    // for all above, create half star
    // those below: split over y=x and make wedge

    val polygon: IndexedSeq[Point] = ???

    (start +: polygon) ++ Vector(end)
  }

  def generateStarPolygon(pointGenerator: Int => Set[Point]): Int => IndexedSeq[Point] = { n =>
    val points = pointGenerator(n).toSeq.map(p => Point(2*p.x, 2*p.y))

    val center = Point(1+n/2, 1+n/2)

    val (leftPoints, rightPoints) = points.partition(_.x < center.x)

    val rightSorted = rightPoints.sortWith { (p1, p2) =>
      (p1.x - center.x) * (p2.y - center.y) < (p1.y - center.y) * (p2.x - center.x) ||
        ((p1.x - center.x) * (p2.y - center.y) == (p1.y - center.y) * (p2.x - center.x) && (p1 - center).length < (p2 -center).length)
    }
    val leftSorted = leftPoints.sortWith { (p1, p2) =>
      (p1.x - center.x) * (p2.y - center.y) < (p1.y - center.y) * (p2.x - center.x) ||
        ((p1.x - center.x) * (p2.y - center.y) == (p1.y - center.y) * (p2.x - center.x) && (p1 - center).length < (p2 -center).length)
    }

    val result = (rightSorted ++ leftSorted).toIndexedSeq.map(p => Point(p.x/2, p.y/2))

    require(result.map(_.x).distinct.size == n && result.map(_.y).distinct.size == n, s"duplicate coordinates used")
    require(!Polygon(result).isSelfIntersecting, s"Generated polygon is self intersecting: $result")
    result
  }

  def generateSimpleDiagonal(n: Int)(implicit random: Random): IndexedSeq[Point] = {
    val begin = Point(1, 1)
    var bottom = IndexedSeq(Point(3, 2))
    var top = IndexedSeq(Point(2, 3))
    for (i <- 4 to n) {
      if (i % 2 == 0) top = (top.head + Vector2D(2, 2)) +: top
      else bottom = (bottom.head + Vector2D(2, 2)) +: bottom
    }
    begin +: (bottom.reverse ++ top)
  }

  def createPolygon(points: Set[Point]): IndexedSeq[Point] = {
    val l = points.minBy(_.x)
    val r = points.maxBy(_.x)
    var leftPoints = Set.empty[Point]
    var rightPoints = Set.empty[Point]
    for (p <- points - l - r) {
      val sign = Math.signum((r.x - l.x) * (p.y - l.y) - (r.y - l.y) * (p.x - l.x))
      if (sign <= 0) leftPoints += p
      else rightPoints += p
    }

    val leftPointsSorted = leftPoints.toIndexedSeq.sortBy(_.x)
    val rightPointsSorted = rightPoints.toIndexedSeq.sortBy(_.x)

    val pointsArray = new Array[Point](points.size)
    pointsArray.update(0, l)
    pointsArray.update(leftPointsSorted.size + 1, r)
    for (i <- leftPointsSorted.indices)
      pointsArray.update(1 + i, leftPointsSorted(i))
    for (i <- rightPointsSorted.indices)
      pointsArray.update(points.size - i - 1, rightPointsSorted(i))

    pointsArray.toIndexedSeq
  }
}

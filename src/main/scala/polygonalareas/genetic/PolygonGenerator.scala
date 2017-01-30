package polygonalareas.genetic

import polygonalareas.generators.PointGenerator
import polygonalareas.{Point, Vector2D}

/**
  * Created by Jordi on 29-1-2017.
  */
object PolygonGenerator {
  def generatePolygonInSquare(n: Int)(pointGenerator: Int => () => IndexedSeq[Point]): () => IndexedSeq[Point] = { () =>
    val innerPoints = Point(1,2) +: Point(2,1) +: (pointGenerator(n - 6)() map (_ + Vector2D(2,2)))
    val p1 = Point(2, 2)
    val p2 = Point(1, n - 1)
    val p3 = Point(n, n)
    val p4 = Point(n - 1, 1)

    val innerPolygon = createPolygon(innerPoints.toSet).map(p => p + Vector2D(2, 2))
    if (innerPolygon.contains(p1)) println(s"inner contains p1")
    if (innerPolygon.contains(p2)) println(s"inner contains p2")
    if (innerPolygon.contains(p3)) println(s"inner contains p3")
    if (innerPolygon.contains(p4)) println(s"inner contains p4")
    val result = innerPolygon.head +: p1 +: p2 +: p3 +: p4 +: innerPolygon.tail
    result
  }

//  def putPointsAt12and21(points: IndexedSeq[Point]): IndexedSeq[Point] = {
//    val i1 = points.indexWhere(_.x == 1)
//    val i2 = points.indexWhere(_.y == 1)
//    // swap so that i1 is at index 0 and i2 is at index 1
//    val swap1 = points.updated(0, points(i1)).updated(i1, points(0))
//    val swap2 = swap1.updated(1, points(i2)).updated(i2, points(1))
//
//    // swap other coordinates so that at index 0 there is (1,2) and at index 1 (2,1)
//    val i3 = points.indexWhere(_.y == 2)
//    val i4 = points.indexWhere(_.x == 2)
//    val swap3 = swap2.updated(i3, Point(swap2(i3).x, swap2(0).y)).updated(0, Point(1, 2))
//    val swap4 = swap3.updated(i4, Point(swap2(i4).y, swap3(0).x)).updated(1, Point(2, 1))
//    swap4
//  }

  def generateDiagonalPointsInSquare(spread: Int = 4, seed: Option[Int] = None)(n: Int): () => IndexedSeq[Point] = { () =>
    def putPointsAt12and21(points: IndexedSeq[Point]): IndexedSeq[Point] = {
      val i1 = points.indexWhere(_.x == 1)
      val i2 = points.indexWhere(_.y == 1)
      // swap so that i1 is at index 0 and i2 is at index 1

      // swap other coordinates so that at index 0 there is (1,2) and at index 1 (2,1)
      ???
    }


    val centerPoints = PointGenerator.generateDiagonalPoints(spread, seed)(n - 4)().map(p => p + Vector2D(2, 2))

    // putPointsAt12and21
    val centerPolygon = PointGenerator.createPolygon(centerPoints.toSet)
    val bottomLeft = centerPoints.sortBy(p => p.x + p.y).head
    val blIndex = centerPolygon.indexOf(bottomLeft)

    //    def centerPolygonFrom

    val p1 = Point(2, 2)
    val p2 = Point(1, n - 1)
    val p3 = Point(n, n)
    val p4 = Point(n - 1, 1)

    // by means of constructing polygon, (1,2) and (2,1) are adjacent. this means p1 can be connected to (2,1) and p2 to (1,2)

    ???
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

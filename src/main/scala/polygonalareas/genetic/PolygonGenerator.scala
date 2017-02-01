package polygonalareas.genetic

import polygonalareas.generators.PointGenerator
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

    val innerPolygon = polygonGenerator(innerPoints.toSet).map(p => p + Vector2D(2, 2))
    if (innerPolygon.contains(p1)) println(s"inner contains p1")
    if (innerPolygon.contains(p2)) println(s"inner contains p2")
    if (innerPolygon.contains(p3)) println(s"inner contains p3")
    if (innerPolygon.contains(p4)) println(s"inner contains p4")
    val result = innerPolygon.head +: p1 +: p2 +: p3 +: p4 +: innerPolygon.tail
    result
  }

  def generatePolygonInSquare(polygonGenerator: Int => IndexedSeq[Point])(n: Int): () => IndexedSeq[Point] = { () =>
    val innerPoints = Point(1, 2) +: Point(2, 1) +: (polygonGenerator(n - 6) map (_ + Vector2D(2, 2)))
    val p1 = Point(2, 2)
    val p2 = Point(1, n - 1)
    val p3 = Point(n, n)
    val p4 = Point(n - 1, 1)

    val innerPolygon = innerPoints.map(p => p + Vector2D(2, 2))
    if (innerPolygon.contains(p1)) println(s"inner contains p1")
    if (innerPolygon.contains(p2)) println(s"inner contains p2")
    if (innerPolygon.contains(p3)) println(s"inner contains p3")
    if (innerPolygon.contains(p4)) println(s"inner contains p4")
    val result = innerPolygon.head +: p1 +: p2 +: p3 +: p4 +: innerPolygon.tail
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

  def generateQuarterStarPolygon(points: Set[Point]): IndexedSeq[Point] = {
    val l = points.minBy(_.x)
    val r = points.maxBy(_.x)
    val n = points.size
    var leftPoints = Set.empty[Point]
    var rightPoints = Set.empty[Point]
    for (p <- points - l - r) {
      val sign = Math.signum((r.x - l.x) * (p.y - l.y) - (r.y - l.y) * (p.x - l.x))
      if (sign <= 0) leftPoints += p
      else rightPoints += p
    }

//    val leftPointsSorted = leftPoints.toIndexedSeq.sortBy(p => p.x)
//    val rightPointsSorted = rightPoints.toIndexedSeq.sortBy(p => -p.y)
    val leftPointsSorted = leftPoints.toIndexedSeq.sortBy(p => ((p.y.toDouble - n / 2) / p.x, p.x*p.x + p.y*p.y))
    val rightPointsSorted = rightPoints.toIndexedSeq.sortBy(p => (-p.y.toDouble / (p.x - n/2), -p.x*p.x - p.y*p.y))

    (l +: leftPointsSorted) ++ (r +: rightPointsSorted)
//    val base = points.minBy(p => p.x*p.x + p.y*p.y)
//    base +: (points - base).toIndexedSeq.sortBy(p => (p.y.toDouble / p.x, p.x*p.x + p.y*p.y))
  }

  def generateSimpleDiagonal(n: Int)(implicit random: Random): IndexedSeq[Point] = {
    val begin = Point(1,1)
    var bottom = IndexedSeq(Point(3,2))
    var top = IndexedSeq(Point(2,3))
    for (i <- 4 to n) {
      if (i % 2 == 0) top = (top.head + Vector2D(2,2)) +: top
      else bottom = (bottom.head + Vector2D(2,2)) +: bottom
    }
    begin +: (bottom.reverse ++ top)
//    var bottom: Seq[Point] = Seq.empty
//    var top: Seq[Point] = Seq.empty
//    var freeY = 1
//    for (x <- 1 to n) {
//      if (random.nextBoolean()) {
//        val y = Math.max(1, Math.min(n, x - 1))
//        bottom = Point(x, y) +: bottom
//      } else {
//        val y = Math.max(1, Math.min(n, x + 1))
//        top = Point(x, y) +: top
//      }
//    }
//    bottom.reverse ++ top
  }


  //  // TODO Generate cross polygon
  //  def generateCrossPolygon(n: Int, seed: Int = Random.nextInt()) = {
  //    val random = new Random(seed)
  //    def generateDiagonal(i: Int, top: Seq[Int] = Seq.empty, bottom: Seq[Int] = Seq.empty)(implicit random: Random): (Seq[Int], Seq[Int]) = {
  //      if (i == 0) (top, bottom)
  //      else if (random.nextBoolean()) generateDiagonal(i - 1, (i + 1) +: top, bottom)
  //      else generateDiagonal(i - 1, top, (i - 1) +: bottom)
  //    }
  //    def tupleReverse(tuple: (Seq[Int], Seq[Int])): (Seq[Int], Seq[Int]) = (tuple._1.reverse, tuple._2.reverse)
  //    val (diagonal1Top, diagonal1Bottom) = generateDiagonal(n / 2)
  //    val (diagonal2Top, diagonal2Bottom) = tupleReverse(generateDiagonal(n - n / 2))
  //


  // generate diagonals d1 on n and d2 on n-1 width
  // mirror one of them in the y=n/2 line
  // interweave them so that d1 end on the uneven numbers and d2 on the even
  // result = d1.map(p => Point(2*p.x+1,2*p.y+1)) ++ d2.map(2*p.x,2*p.y)

  // next, create polygon out of this
  //  }

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

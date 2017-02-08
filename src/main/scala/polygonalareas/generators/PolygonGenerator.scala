package polygonalareas.generators

import polygonalareas.genetic.Polygon
import polygonalareas.{LineSegment, Point, Vector2D, doubleSurface}

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
    val r = Point(n, n)
    val p1 = Point(1, 2)
    val p2 = Point(2, 1)
    val points = pointGenerator(n - 3).map(p => p + Vector2D(2, 2)) + r + p1 + p2
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
    val start = Point(2, 1)
    val end = Point(1, 2)

    // take all points below (x+y==n)
    // for all above, create half star
    // those below: split over y=x and make wedge

    val polygon: IndexedSeq[Point] = ???

    (start +: polygon) ++ Vector(end)
  }

  def generateStarPolygon(pointGenerator: Int => Set[Point]): Int => IndexedSeq[Point] = { n =>
    val points = pointGenerator(n).toSeq.map(p => Point(2 * p.x, 2 * p.y))

    val center = Point(1 + n / 2, 1 + n / 2)

    val (leftPoints, rightPoints) = points.partition(_.x < center.x)

    val rightSorted = rightPoints.sortWith { (p1, p2) =>
      (p1.x - center.x) * (p2.y - center.y) < (p1.y - center.y) * (p2.x - center.x) ||
        ((p1.x - center.x) * (p2.y - center.y) == (p1.y - center.y) * (p2.x - center.x) && (p1 - center).length < (p2 - center).length)
    }
    val leftSorted = leftPoints.sortWith { (p1, p2) =>
      (p1.x - center.x) * (p2.y - center.y) < (p1.y - center.y) * (p2.x - center.x) ||
        ((p1.x - center.x) * (p2.y - center.y) == (p1.y - center.y) * (p2.x - center.x) && (p1 - center).length < (p2 - center).length)
    }

    val result = (rightSorted ++ leftSorted).toIndexedSeq.map(p => Point(p.x / 2, p.y / 2))

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

  def createFractalFromNarrowDiagonal: Int => IndexedSeq[Point] = { n =>
    require(n % 2 == 1)
    val initialDiagonal = generateDiagonalPolygon(PointGenerator.generateNarrowDiagonal()(n))
    val diagonal = initialDiagonal.tail ++ IndexedSeq(initialDiagonal.head)

    val firstHalf = diagonal.take(n / 2)
    val secondHalf = diagonal.drop(n - n / 2)

    var pointsToMoveRight = IndexedSeq.empty[Point]
    var pointsToMoveLeft = IndexedSeq.empty[Point]
    var pointsToMoveDown = IndexedSeq.empty[Point]
    var pointsToMoveUp = IndexedSeq.empty[Point]

    for (i <- 1 to n / 4) {
      if (i % 2 == 1) {
        // move points
        pointsToMoveRight = firstHalf(i) +: pointsToMoveRight
        pointsToMoveLeft = secondHalf(i) +: pointsToMoveLeft
      } else {
        pointsToMoveDown = firstHalf(firstHalf.length - i) +: pointsToMoveDown
        pointsToMoveUp = secondHalf(firstHalf.length - i) +: pointsToMoveUp
      }
    }
    val pointsToStayLeft = firstHalf.indices.filter(_ <= n / 4).map(firstHalf) diff pointsToMoveRight
    val pointsToStayUp = firstHalf.indices.filter(_ > n / 4).map(firstHalf) diff pointsToMoveDown
    val pointsToStayRight = secondHalf.indices.filter(_ <= n / 4).map(secondHalf) diff pointsToMoveLeft
    val pointsToStayDown = secondHalf.indices.filter(_ > n / 4).map(secondHalf) diff pointsToMoveUp

    val (movedRight, movedLeft) = (for ((p1, p2) <- pointsToMoveRight zip pointsToMoveLeft) yield {
      val newP1 = Point(p2.x, p1.y)
      val newP2 = Point(p1.x, p2.y)
      (newP1, newP2)
    }).unzip
    val (movedUp, movedDown) = (for ((p1, p2) <- pointsToMoveUp zip pointsToMoveDown) yield {
      val newP1 = Point(p1.x, p2.y)
      val newP2 = Point(p2.x, p1.y)
      (newP1, newP2)
    }).unzip

    val part1 = pointsToStayLeft ++ movedRight ++ movedDown ++ pointsToStayUp
    val part2 = pointsToStayRight ++ movedLeft ++ movedUp ++ pointsToStayDown
    //    val part1 = pointsToStayLeft ++ movedRight.reverse ++ movedDown.reverse ++ pointsToStayUp
    //    val part2 = pointsToStayRight ++ movedLeft.reverse ++ movedUp.reverse ++ pointsToStayDown
    val result = part1 ++ (Point(n, n) +: part2)

    require(result.size == n, s"Size of generated polygon incorrect: actual size ${result.size}, expected: $n")
    require(result.map(_.x).distinct.size == n, s"duplicate x coordinates (${result.map(_.x).diff(result.map(_.x).distinct)}) used in $result")
    require(result.map(_.y).distinct.size == n, s"duplicate y coordinates (${result.map(_.y).diff(result.map(_.y).distinct)}) used in $result")
    require(!Polygon(result).isSelfIntersecting, s"Generated polygon is self intersecting: $result")
    result
  }

  // TODO Generates a polygon by starting with a triangle and then adding new points, while maintaining the invariants

  def triangleBasedGenerator(points: Set[Point], seed: Int = Random.nextInt()): IndexedSeq[Point] = {
    val random = new Random(seed)
    val n = points.size

    val first = points.minBy(p => (2 * p.x - n) * (2 * p.x - n) + (2 * p.y - n) * (2 * p.y - n))
    val second = (points - first).minBy(p => (p.x - first.x) * (p.x - first.x) + (p.y - first.y) * (p.y - first.y))

    def dist(p1: Point, p2: Point) = (p2 - p1).squareLength
    /**
      * Returns a tuple of (index, surfaceIncrease) that indicates the index
      *
      * @param point
      * @param currentPolygon
      * @return
      */
    def selectIndexToInject(point: Point, currentPolygon: PartialPolygon): (Int, Int) = {
      // edges are safe if connecting 'point' to it does not create intersections
      val safeEdges = currentPolygon.edges.filter { edge =>
        val newEdge1 = LineSegment(edge.p1, point)
        val newEdge2 = LineSegment(edge.p2, point)
        !(currentPolygon intersects newEdge1) && !(currentPolygon intersects newEdge2)
      }
      if (safeEdges.isEmpty) println(s"point:$point, currentPolygon:$currentPolygon")
      val edgeToInjectAt = safeEdges.minBy(ls => doubleSurface(Seq(ls.p1, ls.p2, point)))
      val indexOfEdge = currentPolygon.points.indexOf(edgeToInjectAt.p1)
      val surface = doubleSurface(Seq(currentPolygon.points(indexOfEdge), currentPolygon.points((indexOfEdge + 1) % currentPolygon.points.size), point))
      ((indexOfEdge + 1) % n, surface)
    }

    def expandPartialPolygon(currentPolygon: PartialPolygon, remainingPoints: Set[Point]): IndexedSeq[Point] = {
      if (remainingPoints.isEmpty) {
        currentPolygon.points
      } else {
        val currentPoints = currentPolygon.points
        val potentialNextPoints = for {
          p <- remainingPoints if !(remainingPoints - p).exists(rp => currentPoints.forall(cp => dist(rp, cp) <= dist(p, cp)))
        } yield p
        val ((index, _), point) = potentialNextPoints map (p => (selectIndexToInject(p, currentPolygon), p)) minBy (_._1._2)
        val newPartialPolygon = PartialPolygon(currentPoints.take(index) ++ (point +: currentPoints.drop(index)))
        println(s"recursion with remainingPoints size = ${remainingPoints.size}")
        expandPartialPolygon(newPartialPolygon, remainingPoints - point)
      }
    }

    val result = expandPartialPolygon(PartialPolygon(Vector(first, second)), points - first - second)
    require(result.size == n, s"Size of generated polygon incorrect: actual size ${result.size}, expected: $n")
    require(result.map(_.x).distinct.size == n, s"duplicate x coordinates (${result.map(_.x).diff(result.map(_.x).distinct)}) used in $result")
    require(result.map(_.y).distinct.size == n, s"duplicate y coordinates (${result.map(_.y).diff(result.map(_.y).distinct)}) used in $result")
    require(!Polygon(result).isSelfIntersecting, s"Generated polygon is self intersecting: $result")
    result
  }

  case class PartialPolygon(points: IndexedSeq[Point]) {
    lazy val edges: Seq[LineSegment] = (points zip (points.tail :+ points.head)).map { case (p1, p2) => LineSegment(p1, p2) }.toSeq

    def intersects(edge: LineSegment) = !edges.forall(ls => !(ls intersects edge) || (ls.contains(edge.p1) || ls.contains(edge.p2) ))
  }

}

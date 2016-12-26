package polygonalareas.generators

import polygonalareas.{AnglesSet, _}
import polygonalareas.Implicits.LineSegmentOps

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by m06f791 on 23-12-2016.
  */
class DirectPolygonGenerator(n: Int, seed: Int = Random.nextInt()) {
  val random = new Random(seed)

  def generatePolygon: Option[Polygon] = {
    var points: Seq[Point] = Seq(pickInitialPoint)
    var angles: AnglesSet = AnglesSet.empty
    var remainingPoints: Seq[Point] = for {
      x <- random.shuffle(1 to n).toSeq if !points.exists(_.x == x)
      y <- random.shuffle(1 to n) if !points.exists(_.y == y)
    } yield Point(x, y)

    def isSelfIntersecting(p: Point): Boolean = {
      @tailrec
      def isSelfIntersectingR(ls: LineSegment, pointsToDo: Seq[Point]): Boolean = {
        if (pointsToDo.size <= 1) false
        else if (ls intersects LineSegment(pointsToDo.head, pointsToDo.tail.head)) true
        else isSelfIntersectingR(ls, pointsToDo.tail)
      }
      if (points.size < n - 1) {
        isSelfIntersectingR(LineSegment(p, points.head), points)
      } else {
        isSelfIntersectingR(LineSegment(p, points.head), points) || isSelfIntersectingR(LineSegment(p, points.last), points)
      }
    }
    def createsDuplicateAngle(p: Point): Boolean = {
      if (points.size < n - 1) {
        angles.contains(p - points.head)
      }
      else {
        angles.contains(p - points.head) || angles.contains(p - points.last)
      }
    }
    def addPoint(p: Point): Unit = {
      angles = angles.put(p - points.head)
      remainingPoints = remainingPoints.filter(rp => rp.x != p.x && rp.y != p.y)
      points = p +: points
    }

    // maintain invariants while picking next
    var i = 0
    while (i < n) {
      remainingPoints.find(p => !isSelfIntersecting(p) && !createsDuplicateAngle(p)) match {
        case Some(point) =>
          addPoint(point)
          i += 1
        case None =>
          println(s"stuck at step $i")

          // try to fix polygon

          i = n
      }
    }

    if (points.length == n) Some(Polygon(points.toArray))
    else None
  }

  def pickInitialPoint: Point = {
    val x = 1 + random.nextInt(n / 2)
    val y = 1 + random.nextInt(x)
    Point(x, y)
  }

}

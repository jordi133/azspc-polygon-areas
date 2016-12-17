package polygonalareas

import Implicits.{PointOps, LineSegmentOps}

/**
  * Created by Jordi on 12-12-2016.
  *
  * require(points.head == points.last, "points.head != points.last") (require not allowed in value class)
  */

object Polygon {
  def apply(points: Array[(Int, Int)]): Polygon = {
    new Polygon(points, calculateAngles(points))
  }

  def calculateAngles(points: Array[Point]): AnglesSet = {
    var i = 0
    var result = AnglesSet.empty
    while (i < points.length) {
      val (p1, p2) = (points(i), points((i + 1) % points.length))
      result = result.put(p2 - p1)
      i += 1
    }
    result
  }
}

class Polygon private(val points: Array[(Int, Int)], val angles: AnglesSet) {
  lazy val edges: Seq[LineSegment] = (for ((p1, p2) <- points zip (points.tail :+ points.head)) yield (p1, p2)).toSeq

  /**
    * Using double surface so that we can work with integers
    *
    * @return twice the surface of this polygon
    */
  def doubleSurface: Int = {
    var (dx, dy) = (0, 0)
    var i = 0
    while (i < points.length + 1) {
      dx += points(i % points.length)._1 * points((i + 1) % points.length)._2
      dy += points((i + 1) % points.length)._1 * points(i % points.length)._2
      i += 1
    }
    Math.abs(dy - dx)
  }

  /**
    *
    * @param i can be negative
    * @return
    */
  def getPointModulo(i: Int): Point = points((i + points.length) % points.length)

  /**
    * @return the number of different points in this polygon
    */
  def size = points.length

  /**
    * @return whether this polygon is self intersecting
    */
  lazy val isSelfIntersecting: Boolean = lineSegments.exists(ls1 => lineSegments.exists(ls2 => ls1 != ls2 && (ls1 intersects ls2)))

  /**
    * @return all line segments of this polygon
    */
  lazy val lineSegments = points zip (points.tail :+ points.head)

  override def toString = points.mkString(", ")
}

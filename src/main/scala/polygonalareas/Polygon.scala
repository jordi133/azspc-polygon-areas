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
    while (i < points.length - 1) {
      val (p1, p2) = (points(i), points(i + 1))
      result = result.put(p2 - p1)
      i += 1
    }
    result
  }
}
// TODO: rewrite Polygon so that starting point is no longer repeated at the end (causes trouble when validating angles & intersections etc
class Polygon private(val points: Array[(Int, Int)], val angles: AnglesSet) {
  lazy val edges: Seq[LineSegment] = (for ((p1, p2) <- points.init zip points.tail) yield (p1, p2)).toSeq

  /**
    * Using double surface so that we can work with integers
    *
    * @return twice the surface of this polygon
    */
  def doubleSurface: Int = {
    var (dx, dy) = (0, 0)
    var i = 0
    while (i < points.length) {
      dx += points(i)._1 * points((i + 1) % points.length)._2
      dy += points((i + 1) % points.length)._1 * points(i)._2
      i += 1
    }
    dy - dx
  }

  /**
    *
    * @param i
    * @return
    */
  def getPointModulo(i: Int): Point =     points((i + points.length) % points.length)

  /**
    * @return the number of different points in this polygon
    */
  def size = points.length - 1

  /**
    * @return whether this polygon is self intersecting
    */
  def isSelfIntersecting: Boolean = {
    val lss = lineSegments
    lss.exists(ls1 => lss.exists(ls2 => ls2 != ls2 && (ls1 intersects ls2)))
  }

  /**
    * @return all line segments of this polygon
    */
  def lineSegments = points.init zip points.tail

  override def toString = points.mkString(", ")
}

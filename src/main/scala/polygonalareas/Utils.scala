package polygonalareas

/**
  * Created by Jordi on 14-12-2016.
  */
object Utils {
  /**
    * Determines whether two line segments intersect
    * @param l1
    * @param l2
    * @return
    */
  def linesIntersect(l1: LineSegment, l2: LineSegment): Boolean = {
    val t = ((l2._1 - l1._1) x l1.vector).toDouble / (l1.vector x l2.vector)
    0 <= t && t <= 1
  }

  implicit class PointOps(p: Point) {
    /**
      * Minus operator on points
      * @param other
      * @return
      */
    def -(other: Point): Vector2D = (other._1 - p._1, other._2 - p._2)
  }

  implicit class Vector2DOps(v: Vector2D) {
    /**
      * Cross product
      * @param other
      * @return
      */
    def x(other: Vector2D): Int = v._1 * other._2 - v._2 * other._1
  }

  implicit class LineSegmentOps(ls: LineSegment) {
    def vector: Vector2D = ls._2 - ls._1
  }
}

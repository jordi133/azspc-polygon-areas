package polygonalareas

/**
  * Created by Jordi on 16-12-2016.
  */
object Implicits {

  implicit class Vector2DOps(v: Vector2D) {
    /**
      * @return the cross product of this vector with the other vector
      */
    def x(other: Vector2D): Int = v._1 * other._2 - v._2 * other._1
  }

  implicit class PointOps(p: Point) {
    /**
      * @return the difference between this point and the other point
      */
    def -(other: Point): Vector2D = (other._1 - p._1, other._2 - p._2)

    def x = p._1

    def y = p._2
  }

  implicit class LineSegmentOps(ls: LineSegment) {
    /**
      * @return the direction of this line as a 2D vector
      */
    def vector: Vector2D = ls._2 - ls._1

    def intersects(other: LineSegment): Boolean = {
      val t = ((other._1 - ls._1) x other.vector).toDouble / (ls.vector x other.vector)
      val u = ((other._1 - ls._1) x ls.vector).toDouble / (ls.vector x other.vector).toDouble
      val sharesPoint = sharesPointWith(other)
      val tuValid = 0 <= u && u <= 1 && 0 <= t && t <= 1
      !sharesPoint && tuValid
    }

    private def sharesPointWith(other: LineSegment): Boolean = {
      ls._1 == other._1 || ls._1 == other._2 || ls._2 == other._1 || ls._2 == other._2
    }
  }

}

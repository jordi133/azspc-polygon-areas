import scala.annotation.tailrec

/**
  * Created by Jordi on 14-12-2016.
  */
package object polygonalareas {

  /**
    * A point consists of two integer coordinates, representing a point in a 2D grid
    */
  case class Point(x: Int, y: Int) {
    /**
      * @return the difference between this point and the other point
      */
    def -(other: Point): Vector2D = Vector2D(other.x - x, other.y - y)
  }
  /**
    * A vector consists of two integer components, representing a direction in a 2D grid
    */
  case class Vector2D(x: Int, y: Int) {
    /**
      * @return the cross product of this vector with the other vector
      */
    def x(other: Vector2D): Int = x * other.y - y * other.x
  }

  /**
    * A line segment is defines by a start and end point
    */
  case class LineSegment(p1: Point, p2: Point) {
    /**
      * @return the direction of this line as a 2D vector
      */
    lazy val vector: Vector2D = p2 - p1
  }

  implicit def asPair: Point => (Int, Int) = p => (p.x, p.y)

  def rightTurn(p1: Point, p2: Point, p3: Point): Boolean = {
    val ls1: Vector2D = p2 - p1
    val ls2: Vector2D = p3 - p2
    (ls1 x ls2) <= 0
  }

  def poisson(m: Long, r: Int): Double = {
    @tailrec
    def poissonR(m: Long, r: Int, p: Double, i: Int): Double =
      if (r == i) p else poissonR(m, r, (p * m) / (i + 1), i + 1)

    val p = math.exp(-m)
    poissonR(m, r, p, 0)
  }
}

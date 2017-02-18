import scala.annotation.tailrec

/**
  * Created by Jordi on 14-12-2016.
  */
package object polygonalareas {

  val puzzleSizes = List(5, 7, 11, 17, 23, 29, 37, 47, 59, 71, 83, 97, 113, 131, 149, 167, 191, 223, 257, 293, 331, 373, 419, 467, 521)

  def doubleSurface(points: Seq[Point]): Int = {
    var (dx, dy) = (0, 0)
    var i = 0
    while (i < points.length) {
      dx += points(i).x * points((i + 1) % points.length).y
      dy += points((i + 1) % points.length).x * points(i).y
      i += 1
    }
    Math.abs(dy - dx)
  }

  object Point {
    def apply(tuple: (Int, Int)): Point = Point(tuple._1, tuple._2)
  }

  /**
    * A point consists of two integer coordinates, representing a point in a 2D grid
    */
  case class Point(x: Int, y: Int) {
    /**
      * @return the difference between this point and the other point
      */
    def -(other: Point): Vector2D = Vector2D(other.x - x, other.y - y)

    def +(v: Vector2D): Point = Point(x + v.x, y + v.y)
  }

  /**
    * A vector consists of two integer components, representing a direction in a 2D grid
    */
  case class Vector2D(x: Int, y: Int) {
    /**
      * @return the cross product of this vector with the other vector
      */
    def x(other: Vector2D): Int = x * other.y - y * other.x

    def dot(other: Vector2D): Int = other.x * x + other.y * y

    lazy val length: Double = Math.sqrt(x*x+y*y)
    lazy val squareLength: Long = x*x+y*y
  }

  /**
    * A line segment is defines by a start and end point
    */
  case class LineSegment(p1: Point, p2: Point) {
    /**
      * @return the direction of this line as a 2D vector
      */
    lazy val vector: Vector2D = p2 - p1

    def intersects(other: LineSegment): Boolean = {
      val t = ((other.p1 - p1) x other.vector).toDouble / (vector x other.vector)
      val u = ((other.p1 - p1) x vector).toDouble / (vector x other.vector).toDouble
      val sharesPoint = sharesPointWith(other)
      val tuValid = 0 <= u && u <= 1 && 0 <= t && t <= 1
      !sharesPoint && tuValid
    }

    private def sharesPointWith(other: LineSegment): Boolean = {
      p1 == other.p1 || p1 == other.p2 || p2 == other.p1 || p2 == other.p2
    }

    def contains(p: Point): Boolean = p1 == p || p2 == p
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

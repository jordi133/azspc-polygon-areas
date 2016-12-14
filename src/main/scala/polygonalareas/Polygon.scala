package polygonalareas

/**
  * Created by Jordi on 12-12-2016.
  *
  * require(points.head == points.last, "points.head != points.last") (require not allowed in value class)
  */

class Polygon(val points: Array[(Int, Int)]) {
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
    * @return the number of different points in this polygon
    */
  def size = points.length - 1

  /**
    * @return whether this polygon is self intersecting
    */
  def isSelfIntersecting: Boolean = {
    val lss = lineSegments
    lss.exists(ls1 => lss.exists(ls2 => ls2 != ls2 && Utils.linesIntersect(ls1, ls2)))
  }

  /**
    * @return all line segments of this polygon
    */
  def lineSegments = points.init zip points.tail

  override def toString = points.mkString(", ")
}

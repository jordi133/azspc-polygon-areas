package polygonalareas

/**
  * Created by Jordi on 16-12-2016.
  */
object Implicits {

  implicit class LineSegmentOps(ls: LineSegment) {

    def intersects(other: LineSegment): Boolean = {
      val t = ((other.p1 - ls.p1) x other.vector).toDouble / (ls.vector x other.vector)
      val u = ((other.p1 - ls.p1) x ls.vector).toDouble / (ls.vector x other.vector).toDouble
      val sharesPoint = sharesPointWith(other)
      val tuValid = 0 <= u && u <= 1 && 0 <= t && t <= 1
      !sharesPoint && tuValid
    }

    private def sharesPointWith(other: LineSegment): Boolean = {
      ls.p1 == other.p1 || ls.p1 == other.p2 || ls.p2 == other.p1 || ls.p2 == other.p2
    }
  }

  implicit def PointToTuple(p: Point): (Int, Int) = (p.x, p.y)

}

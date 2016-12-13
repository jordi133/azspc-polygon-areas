package polygonalareas

/**
  * Created by Jordi on 12-12-2016.
  */
class Polygon(val points: Array[(Int, Int)]) extends AnyVal {

  /**
    * Using double surface so that we can work with integers
    * @return
    */
  def doubleSurface: Int = {
    var (dx, dy) = (0, 0)
    for (i <- points.indices) {
      dx += points(i)._1 * points((i + 1) % points.size)._2
      dy += points((i + 1) % points.size)._1 * points(i)._2
    }
    dy - dx
  }
}

package polygonalareas

import scala.util.Random

/**
  * Created by Jordi on 18-12-2016.
  */
class PolygonGenerator(n: Int, seed: Int = Random.nextInt()) {
  val random = new Random(seed)

  /**
    * Generate random points
    * Order clockwise
    * Connect and form polygon
    *
    * @return
    */
  def generateCircularPolygon: Polygon = {


    ???
  }

  /**
    * Generate random points
    * Pick leftmost and rightmost points lm en rm
    * Split points in sets that are above and below line lm-rm
    * Start in lm, connect upper points left-to-right, through rm and then the lower points right-to-left
    *
    * @return
    */
  def generateLinearPolygon: Polygon = {
    val points = generateRandomPoints
    val leftMost = points.minBy{_._1}
    val rightMost = points.maxBy{_._1}

    ???
  }

  def generateFromConvexHull: Polygon = {

    ???
  }

  def generateRandomPoints: Set[Point] = {
    def swap(array: Array[Int], i1: Int, i2: Int) = {
      val temp = array(i1)
      array.update(i1, array(i2))
      array.update(i2, temp)
    }
    val xs = new Array[Int](n)
    val ys = new Array[Int](n)
    for (i <- 0 until n) yield {
      xs.update(i, i + 1)
      ys.update(i, i + 1)
    }
    for (i <- 0 until n) {
      val x = if (n - 1 - i > 0) random.nextInt(n - 1 - i) else 0
      val y = if (n - 1 - i > 0) random.nextInt(n - 1 - i) else 0
      swap(xs, x, n - 1 - i)
      swap(ys, y, n - 1 - i)
    }
    (for (i <- 0 until n) yield (xs(i), ys(i))).toSet
  }
}

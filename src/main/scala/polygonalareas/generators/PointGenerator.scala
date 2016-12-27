package polygonalareas.generators

import polygonalareas.Point

import scala.util.Random

/**
  * Created by Jordi on 23-12-2016.
  */
object PointGenerator {

  def generateDiagonalPoints(n: Int, spread: Int, seed: Int = Random.nextInt()) = {
    val random = new Random(seed)

    val result = Array[Int](n)
    var rest = (1 to n) map identity
    for (x <- 0 until n / 2) {
      val r0 = random.nextInt(Math.min(x + spread / 2, n) - Math.max(x - spread / 2, 0))


    }
  }

  def generateRandomPoints(n: Int, seed: Int = Random.nextInt()): IndexedSeq[Point] = {
    val random = new Random(seed)
    var rest = (1 to n).toVector
    val result = for (x <- 1 to n) yield {
      val i = random.nextInt(rest.length)
      val y = rest(i)
      rest = rest.take(i) ++ rest.drop(i + 1)
      Point(x, y)
    }
    result
  }
}
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

  def generateCrossPoints(n: Int, seed: Int = Random.nextInt()): IndexedSeq[Point] = {
    val random = new Random(seed)
    val center = n/2
    val radiusStep = 4
    def generatePoints(radius: Int, acc: Set[Point]): IndexedSeq[Point] = {
      if (acc.size == n) {
        acc.toIndexedSeq
      }
      else {
        var potentialPoints = random.shuffle(for {
          x <- (Math.max(center - radius, 1) to Math.min( center + radius, n)) filter (i => !acc.exists(p => p.x == i))
          y <- (Math.max(center - radius, 1) to Math.min( center + radius, n)) filter (i => !acc.exists(p => p.y == i))
          point = Point(x, y) if !acc.exists(p => p.x == x || p.y == y)
        } yield point)

        var newPoints = Set.empty[Point]
        while (potentialPoints.nonEmpty) {
          val p = potentialPoints.iterator.next()
          newPoints = newPoints + p
          potentialPoints = potentialPoints.filter(point => !(point.x == p.x || point.y == p.y))
        }
        generatePoints(radius + radiusStep, acc ++ newPoints)
      }
    }

    generatePoints(1, Set.empty)
  }
}
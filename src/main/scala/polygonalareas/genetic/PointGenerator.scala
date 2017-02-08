package polygonalareas.genetic

import polygonalareas.Point

import scala.util.Random

/**
  * Created by Jordi on 4-2-2017.
  */
object PointGenerator {

  def generateDiagonalPoints(spread: Int = 4, seed: Option[Int] = None): (Int) => Set[Point] = { n =>
    val random = new Random(seed.getOrElse(Random.nextInt()))
    val center = n / 2
    def generatePoints(blockCenter: Int, acc: Set[Point]): IndexedSeq[Point] = {
      def getIndexIntervalForRadius: Seq[Int] = Math.max(1, blockCenter - spread) to Math.min(blockCenter + spread, n)
      if (acc.size == n) {
        acc.toIndexedSeq
      } else {
        var potentialPoints = random.shuffle(for {
          x <- getIndexIntervalForRadius filter (i => !acc.exists(p => p.x == i))
          y <- getIndexIntervalForRadius filter (i => !acc.exists(p => p.y == i)) if (x - center) * (y - center) >= 0 || Math.abs(x - y) <= 2 * spread
          point = Point(x, y) if !acc.exists(p => p.x == x || p.y == y)
        } yield point)
        var newPoints = Set.empty[Point]
        while (potentialPoints.nonEmpty) {
          val p = potentialPoints.iterator.next()
          newPoints = newPoints + p
          potentialPoints = potentialPoints.filter(point => !(point.x == p.x || point.y == p.y))
        }
        generatePoints(blockCenter + spread, acc ++ newPoints)
      }
    }

    val result = generatePoints(1, Set.empty).toSet
    require(result.map(_.x).size == n, s"duplicate x coordinate in $result")
    require(result.map(_.y).size == n, s"duplicate y coordinate in $result")
    result
  }

  def generateRandomPoints(seed: Int = Random.nextInt()): (Int) => Set[Point] = { (n) =>
    val random = new Random(seed)
    var rest = (1 to n).toVector
    val result = for (x <- 1 to n) yield {
      val i = random.nextInt(rest.length)
      val y = rest(i)
      rest = rest.take(i) ++ rest.drop(i + 1)
      Point(x, y)
    }
    result.toSet
  }

  def generateCrossPoints(radiusStep: Int = 4, seed: Option[Int] = None): (Int) => Set[Point] = { (n) =>
    val random = new Random(seed.getOrElse(Random.nextInt()))
    val center = n / 2
    def generatePoints(radius: Int, acc: Set[Point]): IndexedSeq[Point] = {
      def getIndexIntervalForRadius: Seq[Int] = {
        val interval1 = 1 to Math.min(center, radius)
        val interval2 = Math.max(center, n - radius) to n
        interval1 ++ interval2
      }
      if (acc.size == n) {
        acc.toIndexedSeq
      }
      else {
        var potentialPoints = random.shuffle(for {
          x <- getIndexIntervalForRadius filter (i => !acc.exists(p => p.x == i))
          y <- getIndexIntervalForRadius filter (i => !acc.exists(p => p.y == i))
          point = Point(x, y)
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

    generatePoints(1, Set.empty).toSet
  }

  def generateNarrowDiagonal(seed: Int = Random.nextInt()): (Int) => Set[Point] = { (n) =>
    val maxEvenNumber = n - (n % 2)
    val diagonal = (for (i <- 1 to maxEvenNumber) yield {
      if (i % 2 == 0) Point(i, i - 1)
      else Point(i, i + 1)
    }).toSet
    val result =
      if (n % 2 == 0) diagonal
      else diagonal + Point(n, n)

    require(result.map(_.x).size == n, s"duplicate x coordinate in $result")
    require(result.map(_.y).size == n, s"duplicate y coordinate in $result")
    result
  }


  def combine(pg1: Int => Set[Point], pg2: Int => Set[Point]): Int => Set[Point] = { n =>
    val points2Size = n / 2
    val points1Size = n - points2Size

    val points1 = pg1(points1Size)
    val points2 = pg2(points2Size)
    val points1Mapped = points1.map(p => Point(2 * p.x - 1, 2 * p.y - 1))
    val points2Mapped = points2.map(p => Point(2 * p.x, 2 * p.y))
    val result  =  points1Mapped ++ points2Mapped
    require(result.map(_.x).size == n, s"duplicate x coordinate in $result")
    require(result.map(_.y).size == n, s"duplicate y coordinate in $result")
    result
  }

}

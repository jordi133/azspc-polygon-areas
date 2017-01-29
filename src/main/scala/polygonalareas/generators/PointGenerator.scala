package polygonalareas.generators

import polygonalareas.{LineSegment, Point, Vector2D}

import scala.util.Random

/**
  * Created by Jordi on 23-12-2016.
  */
object PointGenerator {
  val random = new Random()

  def generateRandomPoints(n: Int, seed: Int = Random.nextInt()): () => IndexedSeq[Point] = { () =>
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

  def generateCrossPoints(radiusStep: Int = 4, seed: Option[Int] = None)(n: Int): () => IndexedSeq[Point] = { () =>
    val random = new Random(seed.getOrElse(this.random.nextInt()))
    val center = n / 2
    def generatePoints(radius: Int, acc: Set[Point]): IndexedSeq[Point] = {
      if (acc.size == n) {
        acc.toIndexedSeq
      }
      else {
        var potentialPoints = random.shuffle(for {
          x <- (Math.max(center - radius, 1) to Math.min(center + radius, n)) filter (i => !acc.exists(p => p.x == i))
          y <- (Math.max(center - radius, 1) to Math.min(center + radius, n)) filter (i => !acc.exists(p => p.y == i))
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

  def generateCircularPoints(seed: Option[Int] = None)(n: Int): () => IndexedSeq[Point] = { () =>
    val random = new Random(seed.getOrElse(this.random.nextInt()))
    val center = n / 2
    val radiusStep = 4
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

    generatePoints(1, Set.empty)
  }

  def generateDiagonalPoints(spread: Int = 4, seed: Option[Int] = None)(n: Int): () => IndexedSeq[Point] = { () =>
    val random = new Random(seed.getOrElse(this.random.nextInt()))
    val center = n / 2
    def generatePoints(blockCenter: Int, acc: Set[Point]): IndexedSeq[Point] = {
      def getIndexIntervalForRadius: Seq[Int] = {
        val interval1 = Math.max(1, blockCenter - spread) to Math.min(blockCenter + spread, center)
        val interval2 = Math.max((n - blockCenter) - spread, center) to Math.min((n - blockCenter) + spread, n)
        interval1 ++ interval2
      }
      if (acc.size == n) {
        acc.toIndexedSeq
      }
      else {
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

    generatePoints(1, Set.empty)
  }

  def generateDoubleDiagonalPoints(spread: Int = 4, seed: Option[Int] = None)(n: Int): () => IndexedSeq[Point] = { () =>
    val random = new Random(seed.getOrElse(this.random.nextInt()))

    val partOne = generateDiagonalPoints(spread, Some(random.nextInt()))(n / 2)()
    val partTwo = generateDiagonalPoints(spread, Some(random.nextInt()))(n - n/2)() map {p => Point(p.x + n/2, n - p.y)}

    partOne ++ partTwo
  }



  def createPolygon(points: Set[Point]): IndexedSeq[Point] = {
    val l = points.minBy(_.x)
    val r = points.maxBy(_.x)
    var leftPoints = Set.empty[Point]
    var rightPoints = Set.empty[Point]
    for (p <- points - l - r) {
      val sign = Math.signum((r.x - l.x) * (p.y - l.y) - (r.y - l.y) * (p.x - l.x))
      if (sign <= 0) leftPoints += p
      else rightPoints += p
    }

    val leftPointsSorted = leftPoints.toIndexedSeq.sortBy(_.x)
    val rightPointsSorted = rightPoints.toIndexedSeq.sortBy(_.x)

    val pointsArray = new Array[Point](points.size)
    pointsArray.update(0, l)
    pointsArray.update(leftPointsSorted.size + 1, r)
    for (i <- leftPointsSorted.indices)
      pointsArray.update(1 + i, leftPointsSorted(i))
    for (i <- rightPointsSorted.indices)
      pointsArray.update(points.size - i - 1, rightPointsSorted(i))

    pointsArray.toIndexedSeq
  }
}
package polygonalareas.genetic

import polygonalareas.{Point, Vector2D, doubleSurface}

import scala.collection.SortedSet
import scala.util.Random

/**
  * Created by Jordi on 29-1-2017.
  */
object Mutater {

  def randomPairOfIndices(n: Int)(implicit random: Random): Seq[Int] = {
    val index1 = random.nextInt(n - 1)
    val index2Try = random.nextInt(n - 2)
    val index2 = if (index2Try == index1) n - 1 else index2Try
    Seq(index1, index2)
  }

  def getIndicesToMutate(n: Int, maxOffSpring: Int, indexOfParallelEdge: Int, indicesToIgnore: Set[Int] = Set.empty)(implicit random: Random): Seq[Seq[Int]] = {
    val possibleIndices = (1 to n) diff (indicesToIgnore.toSeq)
    def randomIndex: Int = {
      val randomIndex1Try = random.nextInt(n - 2)
      if (randomIndex1Try == indexOfParallelEdge) n - 2
      else if (randomIndex1Try == indexOfParallelEdge + 1) n - 1
      else randomIndex1Try
    }
    (for (i <- 1 to maxOffSpring / 2) yield Seq(indexOfParallelEdge, randomIndex)) ++ Seq(
      Seq(indexOfParallelEdge, (indexOfParallelEdge + 1) % n),
      Seq((indexOfParallelEdge + 1) % n, (indexOfParallelEdge + 2) % n)
    )
  }

  def randomPairOfNeighboringIndices(n: Int)(implicit random: Random): Seq[Int] = {
    val index = random.nextInt(n - 1)
    Seq(index, index + 1)
  }

  def mutateGivenIndices(points: IndexedSeq[Point], indicesToMutate: Seq[Int]): Iterator[Seq[Point]] = {
    val mutations = generateMutations(points, indicesToMutate)
    val resultingPointsIterator = mutations map { mutation => updatedPoints(points, mutation, indicesToMutate) } filter (_ != points)
    resultingPointsIterator
  }

  def generateMutations(points: Seq[Point], indicesToMutate: Seq[Int]): Iterator[Seq[(Int, Int)]] = {
    val nrOfPoints = indicesToMutate.length
    val (xs, ys) = (indicesToMutate map (points(_))).unzip

    val mutations: Iterator[Seq[(Int, Int)]] = for {
      xPerm <- (0 until nrOfPoints).permutations
      yPerm <- (0 until nrOfPoints).permutations
    } yield (xPerm zip yPerm) map { case (xi, yi) => (xs(xi), ys(yi)) }

    mutations
  }

  def reorder[T](points: IndexedSeq[T], indexToSwap: Int, indexToInject: Int): (IndexedSeq[T], Seq[Int]) = {
    val swapPoint = points(indexToSwap)
    val partOne = points.take(indexToInject).filter(_ != swapPoint)
    val partTwo = points.drop(indexToInject).filter(_ != swapPoint)
    val n = points.size
    val swapFromPartOne = points.take(indexToInject).contains(swapPoint)
    val impactedIndices =
      if (swapFromPartOne) Seq(indexToSwap, (indexToInject - 2 + n) % n, (indexToInject - 1 + n) % n)
      else Seq((indexToInject + n - 1) % n, indexToInject, indexToSwap)

    val resultingSeq = partOne ++ (swapPoint +: partTwo)

    (resultingSeq, impactedIndices)
  }

  def mutateByAdjustingPoints(points: IndexedSeq[Point], indicesToMutate: Seq[Int]): Iterator[Seq[Point]] = {
    val mutations = generateMutations(points, indicesToMutate)
    val resultingPointsIterator = mutations map { mutation => updatedPoints(points, mutation, indicesToMutate) } filter (_ != points)
    resultingPointsIterator
  }

  def updatedPoints(points: Seq[Point], mutation: Seq[(Int, Int)], indicesToMutate: Seq[Int]): Seq[Point] = indicesToMutate match {
    case i +: is => updatedPoints(points.updated(i, Point(mutation.head)), mutation.tail, is)
    case _ => points
  }

  def swap[T](points: IndexedSeq[T], i1: Int, i2: Int): Seq[T] = {
    val t1 = points(i1)
    val t2 = points(i2)
    points.updated(i1, t2).updated(i2, t1)
  }

  def pickSomeExp(indices: Seq[Int], count: Int, pow: Double = 3, acc: List[Int] = Nil): Seq[Int] =
    if (count > indices.length) {
      indices
    } else if (count > 0) {
      val d = Random.nextDouble()
      val nextIndex = (indices.length * Math.pow(d, pow)).toInt
      pickSomeExp(indices.take(nextIndex) ++ indices.drop(nextIndex + 1), count - 1, pow, indices.drop(nextIndex).head +: acc)
    } else {
      acc
    }

  def triangulatePointsOnSurroundingArea(polygon: Polygon): IndexedSeq[Int] = {
    def angle(v1: Vector2D, v2: Vector2D): Double = {
      val result = 180 * Math.acos((v1 dot v2) / Math.sqrt(v1.squareLength * v2.squareLength)) / Math.PI
      if (result.isNaN)
        println(s"result is NaN for v1=$v1, v2=$v2")
      result
    }

    def calculateAngle(i: Int, currentPolygon: IndexedSeq[Int]): Double = {
//      println(s"calculateAngle for i=$i, currentPolygon=$currentPolygon")
      val n = currentPolygon.length
      val precedingPoint =currentPolygon( (currentPolygon.indexOf(i) + n - 1) % n)
      val followingPoint = currentPolygon((currentPolygon.indexOf(i) + 1) % n)
//      println(s"precedingPoint=$precedingPoint, followingPoint=$followingPoint")
      val v2 = polygon.points(followingPoint) - polygon.points(i)
      val v1 = polygon.points(precedingPoint) - polygon.points(i)
      angle(v1, v2)
    }

    def getNewSortedPoints(sortedPoints: SortedSet[(Int, Double)], currentPolygon: IndexedSeq[Int]): SortedSet[(Int, Double)] = {
      val n = currentPolygon.length
      /* calculates the sinus at indx i for the currentPolygon */

      val indexToRemove = currentPolygon.indexOf(sortedPoints.head._1)
//      println(s"getNewSortedPoints: indexToRemove=${sortedPoints.head._1}")
      val indexToRecalculate1 = currentPolygon((indexToRemove + n - 1) % n)
      val indexToRecalculate2 = currentPolygon((indexToRemove + 1) % n)
      val indexWithAngle1 = (indexToRecalculate1, calculateAngle(indexToRecalculate1, currentPolygon))
      val indexWithAngle2 = (indexToRecalculate2, calculateAngle(indexToRecalculate2, currentPolygon))
//      println(s"indexToRecalculate1 = $indexToRecalculate1, indexToRecalculate2 = $indexToRecalculate2")
      val sp1ToRemove = sortedPoints.find(_._1 == indexToRecalculate1).get
      val sp2ToRemove = sortedPoints.find(_._1 == indexToRecalculate2).get
//      println(s"sp1ToRemove=$sp1ToRemove, sp2ToRemove=$sp2ToRemove, indexWithAngle1=$indexWithAngle1, indexWithAngle2=$indexWithAngle2")
      val result = sortedPoints.tail - sp1ToRemove - sp2ToRemove + indexWithAngle1 + indexWithAngle2
      result
    }
    def nextStep(sortedPoints: SortedSet[(Int, Double)], currentPolygon: IndexedSeq[Int], acc: IndexedSeq[Int]): IndexedSeq[Int] = {
      // 'triangulate' by removing head of that list and recalculating the two adjacent points. Insert these in the sorted list at the right place
      // removed point is first item in the result of this function
//      println(s"nextStep for acc=$acc")
      if (sortedPoints.size < 3) {
        sortedPoints.map(_._1).toIndexedSeq ++ acc
      } else {
        val p = sortedPoints.head
        val newAcc = p._1 +: acc
        // recalculate angles
        val newSortedPoints = getNewSortedPoints(sortedPoints, currentPolygon)
        val indexOfRemovedPointInCurrentPolygon = currentPolygon.indexOf(p._1)
        val newCurrentPolygon = currentPolygon.take(indexOfRemovedPointInCurrentPolygon) ++ currentPolygon.drop(indexOfRemovedPointInCurrentPolygon + 1)
//        println(s"nextStep: newSortedPoints = $newSortedPoints")
//        println(s"nextStep: newCurrentPolygon = $newCurrentPolygon")
        nextStep(newSortedPoints, newCurrentPolygon, newAcc)
      }
    }

    val indicesWithAngle = polygon.points.indices.map { i => (i, calculateAngle(i, polygon.points.indices)) }
    val myOrdering = Ordering.fromLessThan[(Int, Double)] { case (tuple1, tuple2) => tuple1._2 < tuple2._2 || (tuple1._2 == tuple2._2 && tuple1._1 < tuple2._1) }

    val sortedPoints = SortedSet.empty(myOrdering) ++ indicesWithAngle
//    println(s"sortedPoints = ${sortedPoints.mkString(" | ")}")
    nextStep(sortedPoints, polygon.points.indices, Vector.empty)
  }

  def sortPointsOnSurroundingArea(polygon: Polygon): Seq[Int] = {
    def triangleSurface(index1: Int, index2: Int, index3: Int): Int =
      doubleSurface(Seq(polygon.points(index1),polygon.points(index2),polygon.points(index3)))

    def attachSurroundingTriangleSurface(indices: Seq[Int], currentPolygon: IndexedSeq[Int], acc: Seq[(Int, Int)]): Seq[(Int,Int)] = indices match {
      case i +: is =>
        val placeInCurrentPolygon = currentPolygon.indexOf(i)
        val precedingPointIndex = currentPolygon((placeInCurrentPolygon + currentPolygon.length - 1) % currentPolygon.length)
        val followingPointIndex = currentPolygon((placeInCurrentPolygon + 1) % currentPolygon.length)
        val surface = triangleSurface(placeInCurrentPolygon, precedingPointIndex, followingPointIndex)
        val newCurrentPolygon = currentPolygon.take(placeInCurrentPolygon) ++ currentPolygon.drop(placeInCurrentPolygon+1)
        attachSurroundingTriangleSurface(is, newCurrentPolygon, (i, surface) +: acc)
      case Nil =>
        acc
    }

    val triangulation = triangulatePointsOnSurroundingArea(polygon).reverse
    val indicesWithSurface = attachSurroundingTriangleSurface(triangulation, polygon.points.indices, Nil)
    val result = indicesWithSurface.sortBy(-_._2).map(_._1)
    result
  }
}

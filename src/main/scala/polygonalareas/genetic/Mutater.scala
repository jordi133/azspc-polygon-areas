package polygonalareas.genetic

import polygonalareas.Point
import scala.collection.IndexedSeq
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

  def updatedPoints(points: Seq[Point], mutation: Seq[(Int, Int)], indicesToMutate: Seq[Int]): Seq[Point] =  indicesToMutate match {
    case i +: is => updatedPoints(points.updated(i, Point(mutation.head)), mutation.tail, is)
    case _ => points
  }

  def swap[T](points: IndexedSeq[T], i1: Int, i2: Int): Seq[T] = {
    val t1 = points(i1)
    val t2 = points(i2)
    points.updated(i1, t2).updated(i2, t1)
  }
}

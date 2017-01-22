package polygonalareas.generators

import polygonalareas.Point

import scala.collection.IndexedSeq

/**
  * Created by Jordi on 18-1-2017.
  */
object Mutater {
  //
  //  def mutateRandomPoints(points: Seq[Point], nrOfPoints: Int)(implicit random: Random): Iterator[Seq[Point]] = {
  //    val indicesToMutate: Seq[Int] = random.shuffle(points.indices.toList).take(nrOfPoints).sorted
  //
  //    val mutations = generateMutations(points, indicesToMutate)
  //
  //    val resultingPointsIterator = mutations map { mutation => updatedPoints(points, mutation, indicesToMutate) }
  //
  //    resultingPointsIterator
  //  }

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

  def updatedPoints(points: Seq[Point], mutation: Seq[(Int, Int)], indicesToMutate: Seq[Int]): Seq[Point] =  indicesToMutate match {
    case i +: is => updatedPoints(points.updated(i, Point(mutation.head)), mutation.tail, is)
    case _ => points
  }

}

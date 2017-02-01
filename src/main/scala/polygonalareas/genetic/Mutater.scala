package polygonalareas.genetic

import polygonalareas.Point

import scala.collection.IndexedSeq

/**
  * Created by Jordi on 29-1-2017.
  */
class Mutater {

  def pickPointsToMutate(points: Seq[Point]): (Int, Int) = {
    ???
  }

  def mutateByAdjustingPoints(points: IndexedSeq[Point], indicesToMutate: Seq[Int]): Iterator[Seq[Point]] = {
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

  def swap[T](points: IndexedSeq[T], i1: Int, i2: Int): Seq[T] = {
    val t1 = points(i1)
    val t2 = points(i2)
    points.updated(i1, t2).updated(i2, t1)
  }
}

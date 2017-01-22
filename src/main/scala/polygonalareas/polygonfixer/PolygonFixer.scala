package polygonalareas.polygonfixer

import polygonalareas.generators.Mutater
import polygonalareas._

import scala.util.Random

/**
  * Created by Jordi on 21-1-2017.
  *
  * First generates a polygon that could have parallel edges. Next, fix these edges
  */
class PolygonFixer(seed: Int = Random.nextInt()) {
  val random = new Random(seed)
  val offspringOnGoodPolygon = 4

  def evolutionaryFix(initialPoints: IndexedSeq[Point], maximize: Boolean, popsize: Int = 10)(actionOnFound: IndexedSeq[Point] => Unit) = {
    val sortSign = if (maximize) -1 else 1
    var population = Set(Pol(createPolygon(initialPoints.toSet)))
    var done = false
    var count = 0
    while (!done && population.nonEmpty) {
      val newPop = (population flatMap { pol => nextGen(pol)(actionOnFound) }).toIndexedSeq
      val sorted = newPop.sortBy(pol => (-pol.angles.values.size, sortSign * doubleSurface(pol.points))).take(popsize)
      println(s"round $count: newPop size = ${newPop.size}")
      count += 1
      population = sorted.toSet
      val parallelEdges = population.map(initialPoints.size - _.angles.values.size)
      println(s"average parallel edges: ${parallelEdges.sum / parallelEdges.size}")
      println(s"max parallel edges: ${parallelEdges.max}")
      println(s"min parallel edges: ${parallelEdges.min}")
    }
  }

  def nextGen(pol: Pol)(actionOnFound: IndexedSeq[Point] => Unit): Set[Pol] = {
    def randomPairOfIndices: Seq[Int] = {
      val index = random.nextInt(pol.points.length - 1)
      Seq(index, index + 1)
    }
    val angleToIndex = getAngleToIndexMap(pol.points)

    def getIndicesToMutate(indexOfParallelEdge: Int): Seq[Seq[Int]] = Seq(
      Seq(indexOfParallelEdge, (indexOfParallelEdge + 1) % pol.points.length),
      Seq((indexOfParallelEdge + 1) % pol.points.length, (indexOfParallelEdge + 2) % pol.points.length)
    )

    // mutate all parallel edges
    val indicesOfParallelEdges = angleToIndex.values.filter(_.length > 1).flatMap(_.sorted).toSeq
    val indicesToHandle: Seq[Seq[Int]] = randomPairOfIndices +: {
      if (indicesOfParallelEdges.isEmpty) {
        // if no parallel edges exist, then find pair of points to mutate
        actionOnFound(pol.points)
        for (i <- 1 to offspringOnGoodPolygon) yield randomPairOfIndices
      } else {
        // else use parallel edges to mutate
        (indicesOfParallelEdges flatMap { index =>
          getIndicesToMutate(index)
        }).distinct
      }
    }

    val newPolygons = for {
      indices <- indicesToHandle
//      indices <- indicesToHandle.take(5)
      newPolygon <- Mutater.mutateGivenIndices(pol.points, indices) if !Polygon(newPolygon.toArray).isSelfIntersecting
    //      newPolygon <- Mutater.mutateGivenIndices(points, indices) if !hasSelfIntersectingEdgesOnIndices(newPolygon.toIndexedSeq, indices)
    } yield newPolygon.toIndexedSeq

    newPolygons.map(Pol(_)).toSet
  }


  def getAngleToIndexMap(points: IndexedSeq[Point]): Map[Vector2D, Seq[Int]] = {
    def calcMap(i: Int, acc: Map[Vector2D, Seq[Int]]): Map[Vector2D, Seq[Int]] = {
      if (i < points.size) {
        val angle = AnglesSet.normalize(points((i + 1) % points.size) - points(i))
        calcMap(i + 1, acc.updated(angle, i +: acc.getOrElse(angle, Seq.empty)))
      } else {
        acc
      }
    }
    calcMap(0, Map.empty)
  }

  def fixPolygonFromPoints(points: IndexedSeq[Point], depth: Int = 0): Set[IndexedSeq[Point]] = {
    val angleToIndex = getAngleToIndexMap(points)

    def getIndicesToMutate(indexOfParallelEdge: Int): Seq[Seq[Int]] = Seq(
      Seq(indexOfParallelEdge, (indexOfParallelEdge + 1) % points.length),
      Seq((indexOfParallelEdge + 1) % points.length, (indexOfParallelEdge + 2) % points.length)
    )

    // mutate all parallel edges
    val indicesOfParallelEdges = angleToIndex.values.filter(_.length > 1).flatMap(_.sorted).toSeq
    val indicesToHandle: Iterable[Seq[Int]] = (indicesOfParallelEdges flatMap { index =>
      getIndicesToMutate(index)
    }).distinct

    if (indicesToHandle.isEmpty) {
      println(s"found polygon: $points")
      Set(points)
    } else if (depth > 500) {
      Set.empty
    } else {
      println(s"depth=$depth, found ${indicesToHandle.size} parallel edges")
      val newPolygons = for {
        indices <- indicesToHandle
        newPolygon <- Mutater.mutateGivenIndices(points, indices) if !hasSelfIntersectingEdgesOnIndices(newPolygon.toIndexedSeq, indices)
      } yield fixPolygonFromPoints(newPolygon.toIndexedSeq, depth + 1)
      newPolygons.flatten.toSet
    }
  }

  def hasSelfIntersectingEdgesOnIndices(points: IndexedSeq[Point], indices: Seq[Int]): Boolean = {
    def getLineSegment(i: Int) = LineSegment(points(i), points((i + 1) % points.size))
    val linesOnIndices = indices map { i => getLineSegment(i) }

    points.indices.forall { i =>
      indices.contains(i) || !linesOnIndices.exists(ls => ls intersects getLineSegment(i))
    }
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

  case class Pol(points: IndexedSeq[Point]) {
    lazy val angles: Map[Vector2D, Seq[Int]] = getAngleToIndexMap(points)

  }
}

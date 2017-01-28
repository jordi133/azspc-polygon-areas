package polygonalareas.polygonfixer

import polygonalareas.generators.Mutater
import polygonalareas._

import scala.util.Random

/**
  * Created by Jordi on 21-1-2017.
  *
  * First generates a polygon that could have parallel edges. Next, fix these edges
  */
class PolygonFixer(seed: Int = Random.nextInt(), offspringOnGoodPolygon: Int = 4, maxOffSpring: Int = 10) {
  val random = new Random(seed)

  //  def evolutionaryFix(initialPoints: IndexedSeq[Point], maximize: Boolean,
  def evolutionaryFix(pointGenerator: () => IndexedSeq[Point], maximize: Boolean,
                      popsize: Int = 10, maxRoundsWithoutImprovement: Int = 25)
                     (actionOnFound: IndexedSeq[Point] => Unit) = {
    val sortSign = if (maximize) 1 else -1
    val initialPoints = pointGenerator()
    var population = Set(Pol(createPolygon(initialPoints.toSet)))
    //    var population = Set(Pol(createStarPolygon(initialPoints)))
    var roundsWithoutImprovement = 0
    var bestScore = -1
    var count = 0
    val bestEver = (if (maximize) SolutionManager.getMaxSolution(initialPoints.size) else SolutionManager.getMinSolution(initialPoints.size)) match {
      case Some(polygon) => doubleSurface(polygon)
      case _ => -1
    }
    while (population.nonEmpty && (bestScore == -1 || roundsWithoutImprovement < maxRoundsWithoutImprovement)) {
      val newPop = (population.par flatMap { pol => nextGen(pol)(actionOnFound) }).toIndexedSeq
      val sorted = newPop.sortBy(pol => (-pol.angles.values.size, -sortSign * doubleSurface(pol.points))).take(popsize)
      count += 1

      val prevGen =
        if (sorted.size == popsize) {
          Set.empty
        } else {
        population.toSeq.sortBy(pol => (-pol.angles.values.size, -sortSign * doubleSurface(pol.points))).take(popsize - sorted.size)
      }

      population = sorted.toSet ++ prevGen

      if ((sorted ++ prevGen).head.angles.size == initialPoints.size) {
        val newBestPolygon = sorted.head
        val newBestSurface = doubleSurface(newBestPolygon.points)
        val improvement = if (bestScore == -1) newBestSurface else (newBestSurface - bestScore) * sortSign
        if (improvement > 0) {
          roundsWithoutImprovement = 0
          bestScore = newBestSurface
          actionOnFound(newBestPolygon.points)
        } else {
          roundsWithoutImprovement += 1
        }
        println(s"n: ${initialPoints.size} ${if (maximize) "max" else "min"}, round $count: newPop size = ${newPop.size}, bestScore = $bestScore, bestEver: $bestEver, diff with best ever: ${Math.abs(bestScore - bestEver)}")
      } else {
        val parallelEdges = population.map(initialPoints.size - _.angles.values.size)
        println(s"n: ${initialPoints.size} ${if (maximize) "max" else "min"}, round $count: newPop size = ${newPop.size}, min parallel edges: ${parallelEdges.min}")
      }
    }
  }

  def nextGen(pol: Pol)(actionOnFound: IndexedSeq[Point] => Unit): Set[Pol] = {
    def randomPairOfNeighboringIndices: Seq[Int] = {
      val index1 = random.nextInt(pol.points.length - 1)
      val index2Try = random.nextInt(pol.points.length - 2)
      val index2 = if (index2Try == index1) pol.points.length - 1 else index2Try
      Seq(index1, index2)
    }
    def randomPairOfIndices: Seq[Int] = {
      val index = random.nextInt(pol.points.length - 1)
      Seq(index, index + 1)
    }
    val angleToIndex = getAngleToIndexMap(pol.points)

    def getIndicesToMutate(indexOfParallelEdge: Int): Seq[Seq[Int]] = {
      def randomIndex: Int = {
        val randomIndex1Try = random.nextInt(pol.points.size - 2)
        if (randomIndex1Try == indexOfParallelEdge) pol.points.size - 2
        else if (randomIndex1Try == indexOfParallelEdge + 1) pol.points.size - 1
        else randomIndex1Try
      }
      (for (i <- 1 to 10) yield Seq(indexOfParallelEdge, randomIndex)) ++ Seq(
        Seq(indexOfParallelEdge, (indexOfParallelEdge + 1) % pol.points.length),
        //        Seq(indexOfParallelEdge, randomIndex),
        Seq((indexOfParallelEdge + 1) % pol.points.length, (indexOfParallelEdge + 2) % pol.points.length)
      )
    }

    // mutate all parallel edges
    val indicesOfParallelEdges = angleToIndex.values.filter(_.length > 1).flatMap(_.sorted).toSeq
    val indicesToHandle: Seq[Seq[Int]] = {
      if (indicesOfParallelEdges.isEmpty) {
        // if no parallel edges exist, then find pair of points to mutate
        //        actionOnFound(pol.points)
        for (i <- 1 to offspringOnGoodPolygon) yield if (i % 2 == 0) randomPairOfIndices else randomPairOfNeighboringIndices
      } else {
        // else use parallel edges to mutate
        (indicesOfParallelEdges flatMap { index =>
          getIndicesToMutate(index)
        }).distinct.take(maxOffSpring)
      }
    }

    //    val newPolygonsFromSwaps = for {
    //      indices <- indicesToHandle
    //      newPolygon <- Mutater.mutateGivenIndices(pol.points, indices) if !hasSelfIntersectingEdgesOnIndices(newPolygon.toIndexedSeq, indices)
    //    } yield newPolygon.toIndexedSeq

    val newPolygonsFromPointMutations = for {
      indices <- indicesToHandle
      newPolygon <- Mutater.mutateGivenIndices(pol.points, indices) if !hasSelfIntersectingEdgesOnIndices(newPolygon.toIndexedSeq, indices)
    } yield newPolygon.toIndexedSeq

    //    (newPolygonsFromSwaps ++ newPolygonsFromPointMutations).map(Pol(_)).toSet
    newPolygonsFromPointMutations.map(Pol(_)).toSet
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

  def hasSelfIntersectingEdgesOnIndices(points: IndexedSeq[Point], indices: Seq[Int]): Boolean = {
    def getLineSegment(i: Int) = LineSegment(points(i), points((i + 1) % points.size))
    val lineSegments = points.indices.map(getLineSegment)
    def isNonIntersecting(ls: LineSegment): Boolean = lineSegments.forall(lineSegment => !(lineSegment intersects ls) || lineSegment.contains(ls.p1) || lineSegment.contains(ls.p2))

    val indicesToCheck = indices.flatMap(i => Seq((i + points.length - 1) % points.length, i)).distinct
    !indicesToCheck.forall { i => isNonIntersecting(getLineSegment(i)) }
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

  def createStarPolygon(points: IndexedSeq[Point]): IndexedSeq[Point] = {
    val n = points.length
    def distToCenter(p: Point): Double = {
      val dx = Math.abs(p.x - n / 2)
      val dy = Math.abs(p.y - n / 2)
      dx * dx + dy * dy
    }
    val sortedByDistToCenter = points.sortBy { p => distToCenter(p) }

    // start with center
    val center = sortedByDistToCenter.head

    // work clockwise
    val rest = sortedByDistToCenter.tail.sortWith { (p1, p2) =>
      val angle = (p1.x - center.x) * (p2.y - center.y) - (p2.x - center.x) * (p1.y - center.y)
      val p1CloserToCenter = distToCenter(p1) < distToCenter(p2)
      angle < 0 || angle == 0 && p1CloserToCenter
    }

    center +: rest
  }

  case class Pol(points: IndexedSeq[Point]) {
    lazy val angles: Map[Vector2D, Seq[Int]] = getAngleToIndexMap(points)

  }

}

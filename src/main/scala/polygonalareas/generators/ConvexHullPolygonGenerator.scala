package polygonalareas.generators

import polygonalareas._

import scala.util.Random

/**
  * Created by Jordi on 27-12-2016.
  */
class ConvexHullPolygonGenerator(n: Int, seed: Int = Random.nextInt()) {
  val random = new Random(seed)

  def generatePolygons: Set[IndexedSeq[Point]] = {
    val points = PointGenerator.generateRandomPoints(n, random.nextInt())

    val (convexHull, rest) = ConvexHullGenerator.getConvexHullFromSortedPoints(points)
    //    println(s"phase 1: $convexHull - $rest")

    // first find all parallel edges in the convex hull and fix these by injecting points at the right locations
    val fixedSet = fixParallelEdges(convexHull, rest)
    //    println(s"phase 2: fixed set size: ${fixedSet.size}")

    // when no parallel edges are left continue adding points without creating parallel edges
    val result = for ((polygon, rest) <- fixedSet) yield {
      val v = completePolygon(polygon, rest)
      //      println(s"phase 3 result: $v")
      v
    }

    result.flatten
  }

  def findParallelEdges(points: IndexedSeq[Point]): Seq[Seq[Int]] = {
    var vectorToIndicesMap: Map[Vector2D, Seq[Int]] = Map.empty[Vector2D, Seq[Int]]

    for (i <- points.indices) {
      val vector = points((i + 1) % points.length) - points(i)
      val norm = AnglesSet.normalize(vector)

      vectorToIndicesMap = vectorToIndicesMap.updated(norm, i +: vectorToIndicesMap.getOrElse(norm, Nil))
    }
    vectorToIndicesMap.values.filter(_.size > 1).toSeq
  }

  /**
    * @return a set of tuples (polygon, rest) such that no polygon contains parallel edges and every union of
    *         polygon and rest contains the same points as the union of the parameters convexHull and rest
    */
  def fixParallelEdges(convexHull: IndexedSeq[Point], rest: Set[Point]): Set[(IndexedSeq[Point], Set[Point])] = {
    /**
      *
      * @param points        the polygon
      * @param rest          the remaining points
      * @param parallelEdges all parallel edges
      * @param acc           the result so far
      * @return
      */
    def fixR(points: IndexedSeq[Point], rest: Set[Point], parallelEdges: Seq[Seq[Int]],
             acc: Set[(IndexedSeq[Point], Set[Point])] = Set.empty): Set[(IndexedSeq[Point], Set[Point])] = {
      parallelEdges match {
        case (edge1 +: edge2 +: _) +: _ =>
          // there are parallel edges
          // Inject a point from rest in points at index parallelEdges.head.head or parallelEdges.head.tail.head
          val injectedAtEdge1 = injectPointAtEdge(points, rest, parallelEdges.head.head, parallelEdges)
          val injectedAtEdge2 = injectPointAtEdge(points, rest, parallelEdges.head.tail.head, parallelEdges)
          injectedAtEdge1 ++ injectedAtEdge2
        case (edge1 +: Nil) +: remainingParallelEdges =>
          fixR(points, rest, remainingParallelEdges, acc)
        case Nil +: remainingParallelEdges =>
          fixR(points, rest, remainingParallelEdges, acc)
        case Nil =>
          Set((points, rest))
      }
    }
    def injectPointAtEdge(points: IndexedSeq[Point], rest: Set[Point], injectAtEdge: Int,
                          parallelEdges: Seq[Seq[Int]]): Set[(IndexedSeq[Point], Set[Point])] = {
      val result = for (p <- rest if injectingIsAllowed(points, injectAtEdge, p, rest - p)) yield {
        val injectAtIndex = (injectAtEdge + 1) % points.length
        val newPoints = points.take(injectAtIndex) ++ (p +: points.drop(injectAtIndex))
        val newRest = rest - p
        val filteredParallelEdges = parallelEdges.head.filter(_ != injectAtEdge) +: parallelEdges.tail
        val newParallelEdges = filteredParallelEdges.map(seq => seq.map(index => if (index > injectAtEdge) index + 1 else index))
        //        println(s"injectPointAtEdge for points=$points, rest=$rest, injectAtEdge=$injectAtEdge, injectAtIndex=$injectAtIndex, parallelEdges=$parallelEdges")
        //        println(s"newPoints=$newPoints, newRest=$newRest, newParallelEdges=$newParallelEdges")
        fixR(newPoints, newRest, newParallelEdges)
      }
      result.flatten
    }

    val parallelEdges = findParallelEdges(convexHull)

//    println(s"parallelEdges size: ${parallelEdges.size}")
    fixR(convexHull, rest, parallelEdges)
  }

  def completePolygon(points: IndexedSeq[Point], rest: Set[Point]): Set[IndexedSeq[Point]] = {
    if (rest.nonEmpty) {
      // find indices at which p can be injected
      rest.find(p => points.indices.exists(i => injectingIsAllowed(points, i, p, rest - p))) match {
        case None =>
          Set.empty
        case Some(p) =>
          val indices = points.indices.filter(i => injectingIsAllowed(points, i, p, rest - p))
          val subresult = for (i <- indices) yield {
            completePolygon(points.take(i) ++ (p +: points.drop(i)), rest - p)
          }
          subresult.flatten.toSet
      }
    } else {
      Set(points)
    }
  }

  def createPolygon(points: IndexedSeq[Point], rest: Set[Point]): Set[IndexedSeq[Point]] = {

    def createPolygonR(points: IndexedSeq[Point], rest: Set[Point], parallelEdges: Seq[Seq[Int]]): Set[IndexedSeq[Point]] = {
      // if parallelEdges is non-empty, find a point from rest to place at one of those edges

      // if parallelEdges is empty, pick a points from rest and place it, possibly creating a new parallel edge
      ???

    }
    ???


  }


  /**
    * checks whether a point can be injected in the points polygon without introducing a parallel edge and without placing a point
    * in rest outside of the polygon
    */
  def injectingIsAllowed(points: IndexedSeq[Point], indexToInjectAtEdge: Int, pointToInject: Point, rest: Set[Point]): Boolean = {
    val indexToInjectAt = (indexToInjectAtEdge + 1) % points.size
    val placementOk = !rest.exists(p => liesInTriangle(p, points(indexToInjectAtEdge), points((indexToInjectAtEdge + 1) % points.length), pointToInject))
    val newAngle1 = AnglesSet.normalize(pointToInject - points(indexToInjectAtEdge))
    val newAngle2 = AnglesSet.normalize(pointToInject - points((indexToInjectAtEdge + 1) % points.length))
    val angleOk = points.indices.forall { i =>
      val angle = AnglesSet.normalize(points((i + 1) % points.length) - points(i))
      i + 1 != indexToInjectAt && angle != newAngle1 && angle != newAngle2
    }
    placementOk && angleOk
  }

  /**
    * p1 and p2 are part of a polygon. Pre condition: 'point' and 'p3' lie on the same side of p1p2
    */
  def liesInTriangle(point: Point, p1: Point, p2: Point, p3: Point) = {
    val r1 = rightTurn(p1, p2, point)
    val r2 = rightTurn(p2, p3, point)
    val r3 = rightTurn(p3, p1, point)
    r1 == r2 && r2 == r3
  }


}

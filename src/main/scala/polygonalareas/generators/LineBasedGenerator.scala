package polygonalareas.generators

import polygonalareas.{AnglesSet, LineSegment, Point}

import scala.util.Random

/**
  * Created by Jordi on 18-1-2017.
  */
class LineBasedGenerator(n: Int, seed: Int = Random.nextInt()) {
  val random = new Random(seed)


  def getAllPolygons(init: Point): Set[IndexedSeq[Point]] = {

    val tc = LineConstruct(init)

    var pop = Set(tc)

    while (pop.iterator.hasNext && pop.iterator.next.rest.nonEmpty) {
      pop = pop flatMap (_.nextStep(15))
      println(s"pop size: ${pop.size}")
    }

    pop map (_.points)

  }

//  def getPolygons(minimize: Boolean): Set[IndexedSeq[Point]] = {
//
//    val tc = LineConstruct(initialPoint)
//
//    var pop = getAllInitialPoints.map(LineConstruct(_)).toSet
//    val populationSize = pop.size
//    while (pop.iterator.hasNext && pop.iterator.next.rest.nonEmpty) {
//      val newPop = pop flatMap (_.nextStep(5))
//
//      println(s"pop size: ${pop.size}")
//    }
//
//    pop map (_.points)
//
//  }

  /**
    *
    */
  def initialPoint: Point = {
    val x = 1 + random.nextInt(n / 2) // n=5 yields values 1..3
    val y = 1 + random.nextInt(x)
    Point(x, y)
  }

  def getAllInitialPoints: Iterable[Point] = {
    for {
      x <- 1 to n/2
      y <- 1 to x/2
    } yield Point(x,y)
  }

  object LineConstruct {
    private val allPoints = for {
      x <- 1 to n
      y <- 1 to n
    } yield Point(x, y)

    def apply(initialPoint: Point): LineConstruct = {
      require(1 <= initialPoint.x && initialPoint.x <= n && 1 <= initialPoint.y && initialPoint.y <= n, s"$initialPoint exceeds bounds [1,$n]")
      val rest = allPoints filter (p => initialPoint.x != p.x) filter (p => initialPoint.y != p.y)
      println(s"result of init($initialPoint): rest=$rest")
      new LineConstruct(Vector(initialPoint), rest, AnglesSet.empty)
    }
  }

  case class LineConstruct private(points: IndexedSeq[Point], rest: Seq[Point], angles: AnglesSet) {
    /**
      * Invariants:
      * - angles.size == points.size (there are no parallel angles)
      * - there are no intersecting edges
      */

    def addPoint(i: Int, p: Point): LineConstruct = {
      require(i == 0 || i == points.length)
      val newRest = rest filter (point => p.x != point.x && p.y != point.y)
      val newPoints =
        if (i == 0) p +: points
        else points ++ IndexedSeq(p)
      val clearedAngles = angles //.remove(points(i) - points((i + 1) % points.size)) No need to clear as point is added to start or end
      val newAngles =
      if (i == 0) clearedAngles.put(points(i) - p)
      else clearedAngles.put(points(points.size - 1) - p)
      new LineConstruct(newPoints, newRest, newAngles)
    }

    /**
      * Checks whether adding point p at index i breaks invariants
      *
      * @param i
      * @param p
      * @return
      */
    def canAddPoint(i: Int, p: Point): Boolean = {
      //if points(i) - p is parallel with points(i) - points(i+1) then it is also parallel with points(i+1) - p
      require(i == 0 | i == points.length)
      def anglesOk: Boolean =
      if (i == 0) !angles.contains(AnglesSet.normalize(points.head - p))
      else !angles.contains(AnglesSet.normalize(points(i - 1) - p))
      def intersectionsOk: Boolean = {
        val newEdge =
          if (i ==0) LineSegment(p, points.head)
          else LineSegment(p, points(points.length -1))
        !points.indices.exists { index =>
          val ls = LineSegment(points(index), points((index + 1) % points.size))
          index != i && (newEdge intersects ls)
        }
      }
      def pointOk: Boolean = rest.contains(p)

      pointOk && anglesOk && intersectionsOk
    }

    /**
      * Calculates the difference in surface when point p is inserted at index i.
      *
      * Returns positive value for increasing surface iff points are ordered clockwise
      */
    def doubleSurfaceDiffForPoint(i: Int, p: Point) = {
      val v1 = p - points((i + points.size - 1) % points.size)
      val v2 = p - points(i % points.size)
      v1 x v2
    }

    def nextStep(maxAmount: Int): Set[LineConstruct] = {
      if (rest.size == 1) {
        finalStep
      }
      else {
        val diffs: Seq[((Int, Point), Int)] = for {
          i <- Seq(0, points.length)
          p <- rest if canAddPoint(i, p)
        } yield (i, p) -> doubleSurfaceDiffForPoint(i, p)

        val amount = if (maxAmount > 0) maxAmount else diffs.size
        val results = diffs.sortBy(-_._2).map(_._1).take(amount)
//        println(s"results for nextStep of $this: $results")
        results.map { case (i, p) => addPoint(i, p) }.toSet
      }
    }

    def finalStep: Set[LineConstruct] = {
      val p = rest.iterator.next()
      val newAngle1 =  AnglesSet.normalize(points.head - p)
      val newAngle2 = AnglesSet.normalize(points.last - p)
      val anglesOk: Boolean = !angles.contains(newAngle1) && !angles.contains(newAngle2) && newAngle1 != newAngle2
      val intersectionsOk: Boolean = {
        val newEdge1 = LineSegment(p, points.head)
        val newEdge2 = LineSegment(p, points.last)
        !points.indices.tail.exists { index =>
          val ls = LineSegment(points(index - 1), points(index))
          (newEdge1 intersects ls) || (newEdge2 intersects ls)
        }
      }
      val pointOk: Boolean = rest.contains(p)
      val canDoFinalStep = pointOk && anglesOk && intersectionsOk
      if (canDoFinalStep) Set(addPoint(0, p)) else Set.empty
    }

  }

}

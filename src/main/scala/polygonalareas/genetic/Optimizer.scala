package polygonalareas.genetic

import polygonalareas.{SolutionManager, Point, doubleSurface}

import scala.util.Random

/**
  * Created by Jordi on 4-2-2017.
  */
class Optimizer(
                 maxRoundsWithoutImprovement: Int = 25,
                 familyRevitalizations: Int = 0,
                 familySize: Int = 10,
                 nrOfFamilies: Int = 1,
                 maxOffSpring: Int = 25,
                 seed: Int = Random.nextInt()) {
  implicit val random = new Random(seed)

  def optimizeFromPolygon(polygonGenerator: (Int) => IndexedSeq[Point], n: Int, maximize: Boolean)
                         (actionOnFound: IndexedSeq[Point] => Unit) = {
    val sortSign = if (maximize) 1 else -1
    //    var families: Map[Int, IndexedSeq[Polygon]] = (0 until nrOfFamilies map (i => (i, IndexedSeq(Polygon(polygonGenerator(n)))))).toMap
    var families: Map[Int, Family] = (0 until nrOfFamilies map (i => (i, Family(IndexedSeq(Polygon(polygonGenerator(n))))))).toMap
    var familiesDied = 0
    var bestScore = -1
    var count = 0
    val previousBest = (if (maximize) SolutionManager.getMaxSolution(n) else SolutionManager.getMinSolution(n)) match {
      case Some(polygon) => doubleSurface(polygon)
      case _ => -1
    }

    while (families.values.flatMap(_.pop).nonEmpty && count < 15000) {
      count += 1

      // adjust families so that the new generation is represented as much as possible. If size of new generation is smaller than
      // max family size, then fill up with some of the previous generation
      families = families.map { case (i, fam) => i -> fam.nextGen(sortSign) }

      val validPolygons = families.values.flatMap(_.validPolygons)
      if (validPolygons.nonEmpty) {
        val newBestPolygon = validPolygons.minBy { pol => -sortSign * doubleSurface(pol.points) }
        val newBestScore = newBestPolygon.score
        val improvement = if (bestScore == -1) newBestScore else (newBestScore - bestScore) * sortSign
        if (improvement > 0) {
          actionOnFound(newBestPolygon.points)
          bestScore = newBestScore
        }
        println(s"n: $n ${if (maximize) "max" else "min"}, families alive: ${families.size}, fam rev left: ${Math.max(0, familyRevitalizations - familiesDied)}, round $count: newPop size = ${families.values.flatMap(_.pop).size}, bestScore = $bestScore, previousBest: $previousBest, diff with best ever: ${Math.abs(bestScore - previousBest)}")
      } else if (families.values.flatMap(_.pop).nonEmpty) {
        val parallelEdges = families.values.map(_.leastParEdges)
        println(s"n: $n ${if (maximize) "max" else "min"}, round $count: newPop size = ${families.values.flatMap(_.pop).size}, min parallel edges: ${parallelEdges.min}")
      }

      for ((index, family) <- families) {
        if (family.generationsWithoutImprovement >= maxRoundsWithoutImprovement || family.pop.isEmpty) {
          familiesDied += 1
          if (familyRevitalizations > familiesDied) {
            val newPolygon = Polygon(polygonGenerator(n))
            families = families.updated(index, Family(Vector(newPolygon)))
            println(s"Revitalized family, ${familyRevitalizations - familiesDied} left")
          } else {
            families = families.filter(_._1 != index)
          }
        }
      }
    }
  }

  case class Family(pop: Seq[Polygon], generationsWithoutImprovement: Int = 0) {
    lazy val n = pop.head.points.size
    lazy val bestScore = pop.head.score
    lazy val leastParEdges = pop.head.parEdges
    lazy val validPolygons = pop.filter(_.isValid)

    def nextGen(sortSign: Int): Family = {
      def familySelection(pop: Seq[Polygon]): IndexedSeq[Polygon] =
        pop.toIndexedSeq.sortBy(pol => (pol.parEdges, -sortSign * doubleSurface(pol.points))).take(familySize)
      def isScoreImprovement(sortSign: Int, oldScore: Int, newScore: Int): Boolean = (newScore - oldScore) * sortSign > 0

      val newGeneration = familySelection(pop.par.flatMap(_.nextGen(maxOffSpring)).seq)
      val newPop = newGeneration ++ pop.take(familySize - newGeneration.size)

      val improvement =
        if (leastParEdges > 0) newPop.head.parEdges < leastParEdges
        else isScoreImprovement(sortSign, pop.head.score, newPop.head.score)

      val newGenerationsWithoutImprovement = if (improvement) 0 else generationsWithoutImprovement + 1
      Family(newPop, newGenerationsWithoutImprovement)
    }
  }

  /**
    * Returns new generation of families, sorted with best on head position
    *
    * @return
    */
//  def nextGenForFamilies(families: Map[Int, IndexedSeq[Polygon]], sortSign: Int): Map[Int, IndexedSeq[Polygon]] = {
//    def familySelection(pop: Seq[Polygon]): IndexedSeq[Polygon] =
//      pop.toIndexedSeq.sortBy(pol => (-pol.angles.values.size, -sortSign * doubleSurface(pol.points))).take(familySize)
//
//    val newPop = families map { case (index, pop) => (index, familySelection(pop.par.flatMap(_.nextGen(maxOffSpring)).seq)) }
//
//    // adjust families so that the new generation is represented as much as possible. If size of new generation is smaller than
//    // max family size, then fill up with some of the previous generation
//    val newFamilies = newPop.map { case (index, pop) => (index, pop ++ families(index).take(familySize - pop.size)) }
//
//    newFamilies
//  }


}

package polygonalareas

import polygonalareas.generators.{ConvexHullPolygonGenerator, TwoStepPolygonGenerator}

import scala.util.Random

/**
  * Created by Jordi on 18-12-2016.
  *
  * Finds an optimal solution
  *
  */
class Optimizer(n: Int, seed: Int = Random.nextInt()) {
  println(s"Created Optimizer with seed = $seed")

  def search() = {
    // generate set of polygons
    val initialPopulation = generateInitialPopulation(50 * n)

    println(initialPopulation.size)

    // mutate them

    // select candidates for next iteration
  }

  def searchPolygons(amount: Int): Set[Polygon] = {
    val gen = new ConvexHullPolygonGenerator(n, seed)
    var result = Set.empty[Polygon]
    val pointSets = (1 to n).permutations.map(perm => ((1 to n) zip perm).map{case (x,y) => Point(x,y)})
    val stepSize = 100
    val steps = pointSets.grouped(stepSize)
    while (result.size < amount && steps.hasNext) {
      val found = (for (points <- steps.next) yield gen.generatePolygonsWithPoints(points)).flatten
      result = result ++ found.map(points => Polygon(points.toArray)).toSet
    }
    result
  }

  def generateInitialPopulation(tries: Int = 100): Seq[Polygon] = {
    val gen = new TwoStepPolygonGenerator(n, seed)

    val polygons = for (i <- 1 to tries) yield gen.generateStarPolygon
    val result = polygons.filter(p => p.angles.getSet.size == p.size)
    println(s"Generator success (size=$n): ${result.size.toDouble / tries} (${result.size} out of $tries)")
    result
  }

  def createNextGeneration() = {
    ???
  }

}

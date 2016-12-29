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
  val random = new Random(seed)

  def search() = {
    // generate set of polygons
    val initialPopulation = generateInitialPopulation(50 * n)

    println(initialPopulation.size)

    // mutate them

    // select candidates for next iteration
  }

  def searchPolygons(amount: Int): Set[Polygon] = {
    val stepSize = 10
    val base = (1 to n).toVector
    def steps: Seq[IndexedSeq[Int]] = {
      for (_ <- 1 to stepSize) yield random.shuffle(base)
    }
    val gen = new ConvexHullPolygonGenerator(n, seed)
    var result = Set.empty[Polygon]
    //    val pointSets = (1 to n).permutations
    //    val steps = random.shuffle(pointSets.grouped(stepSize))
    while (result.size < amount) {
      val found = (for (step <- steps) yield {
        //        println(s"doing step: $step")
        val points = ((1 to n) zip step).map { case (x, y) => Point(x, y) }
        gen.generatePolygonsWithPoints(points)
      }).flatten
      for (f <- found) println(s"Found (n=$n): $found")
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

package polygonalareas

import polygonalareas.generators.TwoStepPolygonGenerator

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

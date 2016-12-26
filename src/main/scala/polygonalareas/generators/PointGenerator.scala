package polygonalareas.generators

import polygonalareas._

import scala.util.Random

/**
  * Created by Jordi on 23-12-2016.
  */
class DiagonalPointGenerator(spread: Int, seed: Int = Random.nextInt()) {
  val random = new Random(seed)
  def nextPoint(xs: Seq[Int], ys: Seq[Int]): Point= {
    val x = random.nextInt()
    ???
  }
}
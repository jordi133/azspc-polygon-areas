package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.generators.PointGenerator

/**
  * Created by Jordi on 23-1-2017.
  */
class PointGeneratorTest extends WordSpec {
  "PointGenerator" should {
    "generate valid point in cross" in {
      val n = 521
      val pg = PointGenerator
      val points = pg.generateCrossPoints(n)
      val xs = points.map(_.x)
      val ys = points.map(_.y)
      assert(xs.distinct.length == n)
      assert(xs.min == 1)
      assert(xs.max == n)
      assert(ys.distinct.length == n)
      assert(ys.min == 1)
      assert(ys.max == n)
    }
  }
}

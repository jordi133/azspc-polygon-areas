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
      val points = pg.generateCrossPoints(n)()
      val xs = points.map(_.x)
      val ys = points.map(_.y)
      assert(xs.distinct.length == n)
      assert(xs.min == 1)
      assert(xs.max == n)
      assert(ys.distinct.length == n)
      assert(ys.min == 1)
      assert(ys.max == n)
    }
    "generate valid point in circle" in {
      val n = 521
      val pg = PointGenerator
      val points = pg.generateCircularPoints(n)()
      val xs = points.map(_.x)
      val ys = points.map(_.y)
      assert(xs.distinct.length == n)
      assert(xs.min == 1)
      assert(xs.max == n)
      assert(ys.distinct.length == n)
      assert(ys.min == 1)
      assert(ys.max == n)
    }
    "generate valid points in diagonal" in {
      val pg = PointGenerator
      for (i <- 5 to 50) {
        val points = pg.generateDiagonalPoints(i, i / 5)()
        val xs = points.map(_.x)
        val ys = points.map(_.y)
        assert(xs.distinct.length == i)
        assert(xs.min == 1)
        assert(xs.max == i)
        assert(ys.distinct.length == i)
        assert(ys.min == 1)
        assert(ys.max == i)
      }
    }
  }
}

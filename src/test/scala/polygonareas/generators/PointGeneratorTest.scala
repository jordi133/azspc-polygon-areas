package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.Point
import polygonalareas.generators.PointGenerator

/**
  * Created by Jordi on 6-2-2017.
  */
class PointGeneratorTest extends WordSpec {
  "generateNarrowDiagonal" should {
    "generate valid set of points" in {
      assert(PointGenerator.generateNarrowDiagonal()(100).size == 100)
      assert(PointGenerator.generateNarrowDiagonal()(101).size == 101)
      for (Point(x, y) <- PointGenerator.generateNarrowDiagonal()(101)) {
        // Difference between x and y at most 1
        assert((x - y) * (x - y) <= 1)
      }
    }
  }

  "combine" should {
    "result in valid point set" in {
      val pg1 = PointGenerator.generateNarrowDiagonal()
      val pg2 = PointGenerator.generateNarrowDiagonal()
      val result = PointGenerator.combine(pg1, pg2)(101)
      assert(result.size == 101)
    }
  }
}

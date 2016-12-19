package polygonareas

import org.scalatest.WordSpec
import polygonalareas.PolygonGenerator

/**
  * Created by Jordi on 19-12-2016.
  */
class PolygonGeneratorTest extends WordSpec {
  "generateRandomPoints" should {
    "generate a set with coordinates between 1 and n (including)" in {
      val n = 10
      val gen = new PolygonGenerator(n)
      val (xs, ys) = gen.generateRandomPoints.unzip
      assert(xs.min === 1)
      assert(xs.max === n)
      assert(ys.min === 1)
      assert(xs.max === n)
    }
    "generate all different x and y coordinates" in {
      val n = 10
      val gen = new PolygonGenerator(n)
      val (xs, ys) = gen.generateRandomPoints.unzip
      assert(xs.size === n)
      assert(ys.size === n)
    }
  }
}

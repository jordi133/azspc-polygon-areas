package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.Point
import polygonalareas.generators.TwoStepPolygonGenerator

/**
  * Created by Jordi on 19-12-2016.
  */
class TwoStepPolygonGeneratorTest extends WordSpec {

  val n = 10
  val gen = new TwoStepPolygonGenerator(n)
  "generateRandomPoints" should {
    "generate a set with coordinates between 1 and n (including)" in {
      val (xs, ys) = gen.generateRandomPoints.unzip
      assert(xs.min === 1)
      assert(xs.max === n)
      assert(ys.min === 1)
      assert(xs.max === n)
    }
    "generate all different x and y coordinates" in {
      val n = 10
      val gen = new TwoStepPolygonGenerator(n)
      val (xs, ys) = gen.generateRandomPoints.unzip
      assert(xs.size === n)
      assert(ys.size === n)
    }
  }
  "generateLinearPolygon" should {
    "generate a non self intersecting polygon" in {
      val p = gen.generateLinearPolygon
      assert(p.isSelfIntersecting === false)
    }
  }
  "generateStarPolygon" should {
    "generate a non self intersecting polygon" in {
      val p = gen.generateStarPolygon
      assert(p.isSelfIntersecting === false)
    }
  }
}

package polygonareas

import org.scalatest.WordSpec
import polygonalareas.Polygon

/**
  * Created by Jordi on 12-12-2016.
  */
class PolygonTest extends WordSpec {

  "Polygon surface" should {
    "calculate surface for 1x1 box" in {
      val p = new Polygon(Array((0, 0), (0, 1), (1, 1), (1, 0), (0, 0)))
      assert(p.doubleSurface === 2)
    }
    "calculate surface for 2x2 box" in {
      val p = new Polygon(Array((0, 0), (0, 2), (2, 2), (2, 0), (0, 0)))
      assert(p.doubleSurface === 8)
    }
    "calculate surface for 2x2 triangle" in {
      val p = new Polygon(Array((0, 0), (0, 2), (2, 0), (0, 0)))
      assert(p.doubleSurface === 4)
    }
    "calculate surface for polygon" in {
      val p = new Polygon(Array((0, 0), (0, 2), (2, 2), (2, 0), (1, 1), (0, 0)))
      assert(p.doubleSurface === 6)
    }
  }

  "Polygon isSelfIntersecting" should {
    "return false for a square" in {
      val p = new Polygon(Array((0, 0), (0, 1), (1, 1), (1, 0), (0, 0)))
      assert(p.isSelfIntersecting === false)
    }
  }

}

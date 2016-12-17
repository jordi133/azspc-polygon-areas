package polygonareas

import org.scalatest.WordSpec
import polygonalareas.{CoordinateSwapMutation, Polygon}

/**
  * Created by Jordi on 14-12-2016.
  */
class MutationTest extends WordSpec {

  "CoordinateSwapMutation" should {
    "keep the number of points in the polygon the same" in {
      val p = Polygon(Array((0,0),(2,1),(1,2)))
      val pMutated = CoordinateSwapMutation.mutate(p)
      assert(p.size === pMutated.size)
    }
    "generate a valid polygon" in {
      val p = Polygon(Array((4,0),(5,2),(3,4),(2,3),(1,5),(0,1)))
      val pMutated = CoordinateSwapMutation.mutate(p)
      assert(!pMutated.isSelfIntersecting)
    }
    "generate a valid polygon within 10 tries" in {
      val p = Polygon(Array((4,0),(5,2),(3,4),(2,3),(1,5),(0,1)))
      for (i <- 1 to 10) {
        CoordinateSwapMutation.tryMutation(p) match {
          case Some(pMutated) => assert(!pMutated.isSelfIntersecting)
          case None =>
        }
      }
    }
    "generate a valid polygon within 10 tries with triple swap" in {
      val p = Polygon(Array((4,0),(5,2),(3,4),(2,3),(1,5),(0,1)))
      for (i <- 1 to 10) {
        CoordinateSwapMutation.tryMutation(p, 3) match {
          case Some(pMutated) => assert(!pMutated.isSelfIntersecting)
          case None =>
        }
      }
    }
  }
}

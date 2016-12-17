package polygonareas

import org.scalatest.WordSpec
import polygonalareas.{Mutation, Polygon}

/**
  * Created by Jordi on 14-12-2016.
  */
class MutationTest extends WordSpec {

  "CoordinateSwapMutation" should {
    "keep the number of points in the polygon the same" in {
      val p = Polygon(Array((0,0),(2,1),(1,2)))
      val pMutated = Mutation.mutate(p)
      assert(p.size === pMutated.size)
    }
    "generate a valid polygon" in {
      val p = Polygon(Array((4,0),(5,2),(3,4),(2,3),(1,5),(0,1)))
      val pMutated = Mutation.mutate(p)
      assert(!pMutated.isSelfIntersecting)
    }
    "generate a valid polygon within 25 tries" in {
      val p = Polygon(Array((4,0),(5,2),(3,4),(2,3),(1,5),(0,1)))
      var pMutated = Option.empty[Polygon]
      (1 to 25) foreach { i =>
        Mutation.tryMutation(p) match {
          case Some(pm) =>
            println(s"Result after $i attempts: ${pm.points.mkString(", ")}")
            pMutated = Some(pm)
            assert(!pm.isSelfIntersecting)
            assert(pm.angles.getSet.size === pm.size)
          case None =>
        }
      }
      assert(pMutated.isDefined)
    }
    "generate a valid polygon within 100 tries with triple swap" in {
      val p = Polygon(Array((4,0),(5,2),(3,4),(2,3),(1,5),(0,1)))
      for (i <- 1 to 100) {
        Mutation.tryMutation(p, 3) match {
          case Some(pMutated) =>
            println(s"triple swap result after $i attempts: ${pMutated.points.mkString(", ")}")
            assert(!pMutated.isSelfIntersecting)
            assert(pMutated.angles.getSet.size === pMutated.size)
          case None =>
        }
      }
    }
  }
}

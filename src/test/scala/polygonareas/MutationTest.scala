package polygonareas

import org.scalatest.WordSpec
import polygonalareas.{Mutation, Point, Polygon}

/**
  * Created by Jordi on 14-12-2016.
  */
class MutationTest extends WordSpec {

  implicit def tupleToPoint(tuple :(Int, Int)): Point = Point(tuple._1, tuple._2)

  "CoordinateSwapMutation" should {
    "keep the number of points in the polygon the same" in {
      val p = Polygon(Array((0, 0), (2, 1), (1, 2)))
      val pMutated = Mutation.mutate(p)
      assert(p.size === pMutated.size)
    }
    "generate a valid polygon" in {
      val p = Polygon(Array((4, 0), (5, 2), (3, 4), (2, 3), (1, 5), (0, 1)))
      val pMutated = Mutation.mutate(p)
      assert(!pMutated.isSelfIntersecting)
    }
    "generate a valid polygon within 25 times tryMutation" in {
      val p = Polygon(Array((4, 0), (5, 2), (3, 4), (2, 3), (1, 5), (0, 1)))
      var pMutated = Option.empty[Polygon]
      (1 to 50) foreach { i =>
        Mutation.tryMutation(p) foreach { pm =>
          println(s"Result after $i attempts of tryMutation: ${pm.points.mkString(", ")}")
          pMutated = Some(pm)
          assert(!pm.isSelfIntersecting)
          assert(pm.angles.size === pm.size)
        }
      }
      assert(pMutated.isDefined)
    }
    "generate a valid polygon within 100 tries with triple swap" in {
      val p = Polygon(Array((4, 0), (5, 2), (3, 4), (2, 3), (1, 5), (0, 1)))
      for (i <- 1 to 100) {
        Mutation.tryMutation(p, 3).toOption foreach { pMutated =>
          println(s"triple swap result after $i attempts: ${pMutated.points.mkString(", ")}")
          assert(!pMutated.isSelfIntersecting)
          assert(pMutated.angles.size === pMutated.size)
        }
      }
    }
    "triple swap should not result in the original polynom" in {
      val p = Polygon(Array((4, 0), (5, 2), (3, 4), (2, 3), (1, 5), (0, 1)))
      for (i <- 1 to 100) {
        Mutation.tryMutation(p, 3).toOption foreach { pMutated =>
          assert(pMutated.points.toList !== p.points.toList)
        }
      }
    }
  }
}

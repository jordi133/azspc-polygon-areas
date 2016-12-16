package polygonareas

import org.scalatest.WordSpec
import polygonalareas.{CoordinateSwapMutation, Polygon}

/**
  * Created by Jordi on 14-12-2016.
  */
class MutationTest extends WordSpec {

  "CoordinateSwapMutation" should {
    "keep the number of points in the polygon the same" in {
      val p = Polygon(Array((0,0),(2,1),(1,2),(0,0)))
      val pMutated = CoordinateSwapMutation.mutate(p)
      println(s"result: $pMutated")
      assert(p.size === pMutated.size)
    }
  }
}

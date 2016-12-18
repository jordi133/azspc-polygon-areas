package polygonareas

import org.scalatest.WordSpec
import polygonalareas.SolutionManager

/**
  * Created by Jordi on 18-12-2016.
  */
class SolutionManagerTest extends WordSpec {
  "stringToCoordinates" should {
    "return an empty array for an empty string" in {
      assert(SolutionManager.stringToCoordinates("").length === 0)
    }
    "return an an array with one element for (0,0)" in {
      assert(SolutionManager.stringToCoordinates("(0,0)").length === 1)
    }
    "return an an array with two elements for (0,0),(1,1)" in {
      assert(SolutionManager.stringToCoordinates("(0,0),(1,1)").length === 2)
    }
    "return an an array with the correct elements for (0,0),(1,1)" in {
      assert(SolutionManager.stringToCoordinates("(0,0),(1,1)").toList === List((0,0),(1,1)))
    }
  }

}

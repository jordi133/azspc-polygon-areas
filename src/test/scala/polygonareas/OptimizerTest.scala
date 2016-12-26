package polygonareas

import org.scalatest.WordSpec
import polygonalareas.Optimizer

/**
  * Created by Jordi on 19-12-2016.
  */
class OptimizerTest extends WordSpec {
  "generateInitialPopulation" should {
    "generate some polygons" in {
      for (size <- List(5, 7, 11, 17, 23, 29, 37, 47, 59, 71, 83, 97, 113, 131, 149, 167, 191, 223, 257, 293, 331, 373, 419, 467, 521)) {
        val opt = new Optimizer(size)
        val pop = opt.generateInitialPopulation(size * 5)
      }
    }
  }
}

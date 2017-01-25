package polygonareas

import org.scalatest.WordSpec
import polygonalareas.{Optimizer, SolutionManager}

/**
  * Created by Jordi on 19-12-2016.
  */
class OptimizerTest extends WordSpec {

  val solMgr = SolutionManager

  "searching for polygons" should {
    "generate some polygons" in {
      for (size <- List(5, 7, 11, 17, 23, 29, 37, 47, 59, 71, 83, 97, 113, 131, 149, 167, 191, 223, 257, 293, 331, 373, 419, 467, 521).drop(0)) {
        val opt = new Optimizer(size)
//        val pop = opt.generateInitialPopulation(size * 5)
        println(s"searching for polygons of size $size")
        val results = opt.searchPolygons(10)
        for (r <- results) solMgr.addSolution(r.points.toSeq)
        solMgr.saveToFile()
      }
    }
  }
}

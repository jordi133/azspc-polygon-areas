package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.generators.DirectPolygonGenerator

/**
  * Created by m06f791 on 23-12-2016.
  */
class DirectPolygonGeneratorTest extends WordSpec {

  "generatePolygon" should {
    "generate a correct polygon" in {
      val gen = new DirectPolygonGenerator(25, 0)
      var successRate = 0
      val count = 50
      for (i <- 1 to count) {
        gen.generatePolygon match {
          case Some(p) =>
            println(s"Generated $p")
            assert(!p.isSelfIntersecting)
            assert(p.angles.getSet.size === p.size)
            successRate += 1
          case None =>
        }
      }
      println(s"Success rate: $successRate / $count")

    }
  }
}

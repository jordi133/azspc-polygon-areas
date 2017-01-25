package polygonareas.polygonfixer

import org.scalatest.WordSpec
import polygonalareas._

/**
  * Created by Jordi on 25-1-2017.
  */
class PackageObjectTest extends WordSpec {
  "doubleSurface" should {
    "calculate the surface correctly" in {
      val points = Seq(Point(1,1), Point(2,3), Point(3,2))
      assert(doubleSurface(points) === 3)
    }
  }

}

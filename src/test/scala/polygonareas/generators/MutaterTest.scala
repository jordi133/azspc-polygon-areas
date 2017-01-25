package polygonareas.generators

import org.scalatest.WordSpec
import polygonalareas.Point
import polygonalareas.generators.Mutater

import scala.util.Random

/**
  * Created by Jordi on 21-1-2017.
  */
class MutaterTest extends WordSpec {

  implicit val random = new Random(0)

  "updatedPoints" should {
    "update points in original" in {
      val points = Seq(Point(1, 1), Point(2, 2), Point(3, 3), Point(4, 4))
      val mutation = Seq((2, 3), (3, 2))
      val mutated = Mutater.updatedPoints(points, mutation, Seq(1, 2))
      assert(mutated === Seq(Point(1,1), Point(2,3), Point(3,2), Point(4,4)))
    }
  }

  "generateMutations" should {
    "generate an iterator for all mutations" in {
      val points = Seq(Point(1, 1), Point(2, 2), Point(3, 3))
      val mutations = Mutater.generateMutations(points, Seq(1, 2)).toSet
      val m1 = Vector((2, 2), (3, 3))
      val m2 = Vector((2, 3), (3, 2))
      val m3 = Vector((3, 2), (2, 3))
      val m4 = Vector((3, 3), (2, 2))
      assert(mutations === Set(m1, m2, m3, m4))
    }
  }

  "swapMutation" should {
    "swap two indices" in {
      val points = Vector(1,2,3,4,5,6,7)
      assert(Mutater.swap(points, 3,4) === Seq(1,2,3,5,4,6,7))
    }
  }

}

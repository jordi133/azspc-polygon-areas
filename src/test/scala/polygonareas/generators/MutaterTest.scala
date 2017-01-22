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

//  "mutateRandomPoints" should {
//    "mutate a seq of points" in {
//      val points = Seq(Point(1, 1), Point(2, 2), Point(3, 3))
//      val mutations = Mutater.mutateRandomPoints(points, 2).toSeq
//      val m1 = Seq(Point(1, 1), Point(2, 2), Point(3, 3))
//      val m2 = Seq(Point(1, 1), Point(2, 3), Point(3, 2))
//      val m3 = Seq(Point(1, 1), Point(3, 2), Point(2, 3))
//      val m4 = Seq(Point(1, 1), Point(3, 3), Point(2, 2))
//      val m6 = Seq(Point(1, 3), Point(2, 2), Point(3, 1))
//      val m7 = Seq(Point(3, 1), Point(2, 2), Point(1, 3))
//      val m8 = Seq(Point(3, 3), Point(2, 2), Point(1, 1))
//      val m10 =Seq(Point(1, 2), Point(2, 1), Point(3, 3))
//      val m11 =Seq(Point(2, 1), Point(1, 2), Point(3, 3))
//      val m12 =Seq(Point(2, 2), Point(1, 1), Point(3, 3))
//      val expected = Set(m1, m2, m3, m4,  m6, m7, m8,  m10, m11, m12)
//      for (m <- mutations) assert(expected.contains(m))
//      assert(mutations.size === expected.size)
//    }
//  }

}

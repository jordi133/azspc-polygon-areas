package polygonalareas

import scala.util.Random

import polygonalareas.Utils._

/**
  * Created by Jordi on 14-12-2016.
  */
trait Mutation {
  def mutate(p: Polygon): Polygon

  def test(p: Polygon): Boolean = {
    def hasParallelEdges: Boolean = {
      val angles = for (ls <- p.lineSegments.toSeq) yield ls.vector.angleCos
      angles.distinct.length != angles.length
    }
// todo fix has parallel edges
    hasParallelEdges && !p.isSelfIntersecting
  }
}

object PointSwapMutation extends Mutation {
  override def mutate(p: Polygon): Polygon = {
    ???
  }
}

object CoordinateSwapMutation extends Mutation {
  override def mutate(p: Polygon): Polygon = {
    val newPoints = new Array[Point](p.size + 1)
    p.points.copyToArray(newPoints)
    val result = new Polygon(newPoints)

    var attempts = 0
    val maxAttempts = 100
    var done = false
    while (!done) {
      p.points.copyToArray(newPoints)
      val i1 = Random.nextInt(p.size)
      val p1 = p.points(i1)
      val i2a = Random.nextInt(p.size - 1)
      val i2 = if (i2a == i1) p.size - 1 else i2a
      val p2 = p.points(i2)

      newPoints(i1) = (p1._1, p2._2)
      newPoints(i2) = (p2._1, p1._2)

      if (!test(result)) {
        attempts += 1
        if (attempts == maxAttempts) throw new RuntimeException(s"Cannot find valid mutation on $p")
      } else {
        done = true
      }
    }

    result
  }
}

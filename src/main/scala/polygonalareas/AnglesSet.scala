package polygonalareas

import AnglesSet._

/**
  * Created by Jordi on 16-12-2016.
  */
object AnglesSet {

  def empty = new AnglesSet(Set.empty[Vector2D])

  /**
    * @return v normalized by dividing both components by the gcd and flipping
    *         direction to make y (and x if possible) positive while keeping the angle the same
    */
  def normalize(v: Vector2D): Vector2D = {
    if (v.x == 0) {
      Vector2D(0, 1)
    } else if (v.y == 0) {
      Vector2D(1, 0)
    } else {
      val Vector2D(x, y) = v
      val (v1abs, v2abs) = (Math.abs(v.x), Math.abs(v.y))
      val a = gcd(v1abs, v2abs)
      if (x * y > 0) {
        Vector2D(v1abs / a, v2abs / a)
      } else {
        Vector2D(-v1abs / a, v2abs / a)
      }
    }
  }

  def gcd(a: Int, b: Int): Int = {
    var x = a
    var y = b
    while (y != 0) {
      val t = y
      y = x % y
      x = t
    }
    x
  }
}

/**
  * Class keeps track of angles of a polygon, storing the normalized vector
  *
  * @param angles
  */
class AnglesSet private(angles: Set[Vector2D]) {

  def contains(v: Vector2D): Boolean = angles.contains(normalize(v))

  def put(v: Vector2D): AnglesSet = new AnglesSet(angles + normalize(v))

  def removeAll(vs: Set[Vector2D]) = new AnglesSet(angles diff vs.map(normalize))

  def getSet = angles
}

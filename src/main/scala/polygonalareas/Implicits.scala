package polygonalareas

/**
  * Created by Jordi on 16-12-2016.
  */
object Implicits {

  implicit def PointToTuple(p: Point): (Int, Int) = (p.x, p.y)

}

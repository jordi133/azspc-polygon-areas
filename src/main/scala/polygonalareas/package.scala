/**
  * Created by Jordi on 14-12-2016.
  */
package object polygonalareas {
  /**
    * A point consists of two integer coordinates, representing a point in a 2D grid
    */
  type Point = (Int, Int)
  /**
    * A vector consists of two integer components, representing a direction in a 2D grid
    */
  type Vector2D = (Int, Int)
  /**
    * A line segment is defines by a start and end point
    */
  type LineSegment = (Point, Point)
}
package polygonalareas

/**
  * Created by Jordi on 18-12-2016.
  */
class PolygonGenerator(n: Int) {

  /**
    * Generate random points
    * Order clockwise
    * Connect and form polygon
    * @return
    */
  def generateCircularPolygon: Polygon = {

    ???
  }

  /**
    * Generate random points
    * Pick leftmost and rightmost points lm en rm
    * Split points in sets that are above and below line lm-rm
    * Start in lm, connect upper points left-to-right, through rm and then the lower points right-to-left
    * @return
    */
  def generateLinearPolygon: Polygon = {

    ???
  }
}

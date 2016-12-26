package polygonalareas

import java.io.PrintWriter

import scala.io.Source
import scala.util.Try

/**
  * Created by Jordi on 18-12-2016.
  *
  * Object keeps track of solutions, saving candidate solutions if they are better than what has been found so far
  */
object SolutionManager {
  val filename = "solutions.txt"

  var polygons: Map[Int, (Option[Polygon], Option[Polygon])] = {
    var result = Map.empty[Int, (Option[Polygon], Option[Polygon])]
    for (line <- Source.fromFile(filename).getLines()) yield {
      val polygon = Polygon(stringToCoordinates(line.filter(c => c != ';')))
      result.get(polygon.size) match {
        case None =>
          result = result.updated(polygon.size, (Some(polygon), Some(polygon)))
        case Some((None, None)) =>
          result = result.updated(polygon.size, (Some(polygon), Some(polygon)))
        case Some((Some(min), None)) =>
          if (polygon.doubleSurface < min.doubleSurface) result = result.updated(polygon.size, (Some(polygon), Some(min)))
          else result = result.updated(polygon.size, (Some(min), Some(polygon)))
        case Some((None, Some(max))) =>
          if (polygon.doubleSurface > max.doubleSurface) result = result.updated(polygon.size, (Some(max), Some(polygon)))
          else result = result.updated(polygon.size, (Some(polygon), Some(max)))
        case Some((Some(min), Some(max))) =>
          if (polygon.doubleSurface > max.doubleSurface) result = result.updated(polygon.size, (Some(min), Some(polygon)))
          if (polygon.doubleSurface < min.doubleSurface) result = result.updated(polygon.size, (Some(polygon), Some(max)))
          println(s"found three polygons for size ${polygon.size}")
      }
    }
    result
  }

  def addSolution(polygon: Polygon) = {
    polygons.get(polygon.size) match {
      case None =>
        polygons = polygons.updated(polygon.size, (Some(polygon), Some(polygon)))
      case Some((None, None)) =>
        polygons = polygons.updated(polygon.size, (Some(polygon), Some(polygon)))
      case Some((Some(min), None)) =>
        if (polygon.doubleSurface < min.doubleSurface) polygons = polygons.updated(polygon.size, (Some(polygon), None))
        else polygons = polygons.updated(polygon.size, (Some(min), Some(polygon)))
      case Some((None, Some(max))) =>
        if (polygon.doubleSurface > max.doubleSurface) polygons = polygons.updated(polygon.size, (None, Some(polygon)))
        else polygons = polygons.updated(polygon.size, (Some(polygon), Some(max)))
      case Some((Some(min), Some(max))) =>
        if (polygon.doubleSurface > max.doubleSurface) polygons = polygons.updated(polygon.size, (Some(min), Some(polygon)))
        if (polygon.doubleSurface < min.doubleSurface) polygons = polygons.updated(polygon.size, (Some(polygon), Some(max)))
    }
  }

  def saveToFile(): Unit = {
    val polygonStrings = for (size <- polygons.keys) yield {
      polygons.get(size) match {
        case None =>
          ""
        case Some((None, None)) =>
          ""
        case Some((Some(min), None)) =>
          min.points.mkString(",")
        case Some((None, Some(max))) =>
          max.points.mkString(",")
        case Some((Some(min), Some(max))) =>
          min.points.mkString(",") + ";\n" + max.points.mkString(",")
      }
    }

    val result = polygonStrings.filter(str => str.length > 0).mkString("\n")
    new PrintWriter(filename) {
      Try(write(result))
      close()
    }
  }

  def stringToCoordinates(s: String): Array[Point] = {
    if (s.isEmpty) {
      new Array[Point](0)
    } else {
      val coordinates = s.split("\\),\\(") map { str: String =>
        val (x, y) = str.splitAt(str.indexOf(','))
        Point(x.filter(c => c.isDigit).toInt, y.filter(c => c.isDigit).toInt)
      }
      val result = new Array[Point](coordinates.length)
      for (i <- coordinates.indices) result.update(i, coordinates(i))
      result
    }
  }

  def getMinSolution(n: Int): Option[Polygon] = polygons.get(n).flatMap(_._1)

  def getMaxSolution(n: Int): Option[Polygon] = polygons.get(n).flatMap(_._2)
}

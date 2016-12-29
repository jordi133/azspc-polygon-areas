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
    val lines = Source.fromFile(filename).getLines()
    for (line <- lines) {
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
      }
    }
    result
  }

  def addSolution(polygon: Polygon) = {
    require(polygon.angles.getSet.size == polygon.size, s"polygon has parallel edges: $polygon")
    require(!polygon.isSelfIntersecting, s"polygon is self intersecting: $polygon")
    polygons.get(polygon.size) match {
      case None =>
        polygons = polygons.updated(polygon.size, (Some(polygon), Some(polygon)))
        println(s"1Updated min and max solution for size=${polygon.size}")
      case Some((None, None)) =>
        polygons = polygons.updated(polygon.size, (Some(polygon), Some(polygon)))
        println(s"2Updated min and max solution for size=${polygon.size}")
      case Some((Some(min), None)) =>
        if (polygon.doubleSurface < min.doubleSurface) {
          polygons = polygons.updated(polygon.size, (Some(polygon), Some(min)))
          println(s"3Updated min and max solution for size=${polygon.size}, score increased by raw ${min.doubleSurface-polygon.doubleSurface}/2")
        }
        else {
          polygons = polygons.updated(polygon.size, (Some(min), Some(polygon)))
          println(s"4Updated max solution for size=${polygon.size}, score increased by raw ${polygon.doubleSurface-min.doubleSurface}/2")
        }
      case Some((None, Some(max))) =>
        if (polygon.doubleSurface > max.doubleSurface) {
          polygons = polygons.updated(polygon.size, (Some(max), Some(polygon)))
          println(s"5Updated min and max solution for size=${polygon.size}, score increased by raw ${polygon.doubleSurface-max.doubleSurface}/2")
        }
        else {
          polygons = polygons.updated(polygon.size, (Some(polygon), Some(max)))
          println(s"6Updated min solution for size=${polygon.size}, score increased by raw ${max.doubleSurface-polygon.doubleSurface}/2")
        }
      case Some((Some(min), Some(max))) =>
        if (polygon.doubleSurface > max.doubleSurface) {
          polygons = polygons.updated(polygon.size, (Some(min), Some(polygon)))
          println(s"7Updated max solution for size=${polygon.size}, score increased by raw ${polygon.doubleSurface-max.doubleSurface}/2")
        }
        if (polygon.doubleSurface < min.doubleSurface) {
          polygons = polygons.updated(polygon.size, (Some(polygon), Some(max)))
          println(s"8Updated min solution for size=${polygon.size}, score increased by raw ${min.doubleSurface-polygon.doubleSurface}/2")
        }
    }
  }

  def saveToFile(): Unit = this.synchronized {
    val polygonStrings = for (size <- polygons.keys.toIndexedSeq.sorted) yield {
      polygons.get(size) match {
        case None =>
          ""
        case Some((None, None)) =>
          ""
        case Some((Some(min), None)) =>
          min.points.map(asPair).mkString(",")
        case Some((None, Some(max))) =>
          max.points.map(asPair).mkString(",")
        case Some((Some(min), Some(max))) =>
          min.points.map(asPair).mkString(",") + ";\n" + max.points.map(asPair).mkString(",")
      }
    }

    val result = polygonStrings.filter(str => str.length > 0).mkString(";\n")
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

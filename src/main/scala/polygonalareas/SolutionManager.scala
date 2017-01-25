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
  val filename = "solutions-polygonfixer.txt"

  var polygons: Map[Int, (Option[Seq[Point]], Option[Seq[Point]])] = {
    var result = Map.empty[Int, (Option[Seq[Point]], Option[Seq[Point]])]
    val lines = Source.fromFile(filename).getLines()
    for (line <- lines) {
      val polygon = stringToCoordinates(line.filter(c => c != ';'))
      result.get(polygon.size) match {
        case None =>
          result = result.updated(polygon.size, (Some(polygon), Some(polygon)))
        case Some((None, None)) =>
          result = result.updated(polygon.size, (Some(polygon), Some(polygon)))
        case Some((Some(min), None)) =>
          if (doubleSurface(polygon) < doubleSurface(min)) result = result.updated(polygon.size, (Some(polygon), Some(min)))
          else result = result.updated(polygon.size, (Some(min), Some(polygon)))
        case Some((None, Some(max))) =>
          if (doubleSurface(polygon) > doubleSurface(max)) result = result.updated(polygon.size, (Some(max), Some(polygon)))
          else result = result.updated(polygon.size, (Some(polygon), Some(max)))
        case Some((Some(min), Some(max))) =>
          if (doubleSurface(polygon) > doubleSurface(max)) result = result.updated(polygon.size, (Some(min), Some(polygon)))
          if (doubleSurface(polygon) < doubleSurface(min)) result = result.updated(polygon.size, (Some(polygon), Some(max)))
      }
    }
    println(s"Best scores:\n${result.map{case (k,v) => (k, doubleSurface(v._1.get), doubleSurface(v._2.get))}.mkString("\n")}")
    result
  }
  

  def addSolution(polygon: Seq[Point]) = {
    val p = Polygon(polygon.toArray)
    require(p.angles.size == p.size, s"polygon has parallel edges: $polygon")
    require(!p.isSelfIntersecting, s"polygon is self intersecting: $polygon")
    polygons.get(polygon.size) match {
      case None =>
        polygons = polygons.updated(polygon.size, (Some(polygon), Some(polygon)))
        println(s"1Updated min and max solution for size=${polygon.size}")
        saveToFile()
      case Some((None, None)) =>
        polygons = polygons.updated(polygon.size, (Some(polygon), Some(polygon)))
        println(s"2Updated min and max solution for size=${polygon.size}")
        saveToFile()
      case Some((Some(min), None)) =>
        if (doubleSurface(polygon) < doubleSurface(min)) {
          polygons = polygons.updated(polygon.size, (Some(polygon), Some(min)))
          println(s"3Updated min and max solution for size=${polygon.size}, score improved by raw ${doubleSurface(min) - doubleSurface(polygon)}/2")
          saveToFile()
        }
        else {
          polygons = polygons.updated(polygon.size, (Some(min), Some(polygon)))
          println(s"4Updated max solution for size=${polygon.size}, score improved by raw -${doubleSurface(polygon) - doubleSurface(min)}/2")
          saveToFile()
        }
      case Some((None, Some(max))) =>
        if (doubleSurface(polygon) > doubleSurface(max)) {
          polygons = polygons.updated(polygon.size, (Some(max), Some(polygon)))
          println(s"5Updated min and max solution for size=${polygon.size}, score improved by raw ${doubleSurface(polygon) - doubleSurface(max)}/2")
          saveToFile()
        }
        else {
          polygons = polygons.updated(polygon.size, (Some(polygon), Some(max)))
          println(s"6Updated min solution for size=${polygon.size}, score improved by raw ${doubleSurface(max) - doubleSurface(polygon)}/2")
          saveToFile()
        }
      case Some((Some(min), Some(max))) =>
        if (doubleSurface(polygon) > doubleSurface(max)) {
          polygons = polygons.updated(polygon.size, (Some(min), Some(polygon)))
          println(s"7Updated max solution for size=${polygon.size}, score improved by raw ${doubleSurface(polygon) - doubleSurface(max)}/2")
          saveToFile()
        }
        if (doubleSurface(polygon) < doubleSurface(min)) {
          polygons = polygons.updated(polygon.size, (Some(polygon), Some(max)))
          println(s"8Updated min solution for size=${polygon.size}, score improved by raw -${doubleSurface(min) - doubleSurface(polygon)}/2")
          saveToFile()
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
          min.map(asPair).mkString(",")
        case Some((None, Some(max))) =>
          max.map(asPair).mkString(",")
        case Some((Some(min), Some(max))) =>
          min.map(asPair).mkString(",") + ";\n" + max.map(asPair).mkString(",")
      }
    }

    val result = polygonStrings.filter(str => str.length > 0).mkString(";\n")
    new PrintWriter(filename) {
      Try(write(result))
      close()
    }
  }

  def stringToCoordinates(s: String): Seq[Point] = {
    if (s.isEmpty) {
      Seq.empty
    } else {
      val coordinates = s.split("\\),\\(") map { str: String =>
        val (x, y) = str.splitAt(str.indexOf(','))
        Point(x.filter(c => c.isDigit).toInt, y.filter(c => c.isDigit).toInt)
      }
//      val result = new Array[Point](coordinates.length)
//      for (i <- coordinates.indices) result.update(i, coordinates(i))
      coordinates.toSeq
    }
  }

  def getMinSolution(n: Int): Option[Seq[Point]] = polygons.get(n).flatMap(_._1)

  def getMaxSolution(n: Int): Option[Seq[Point]] = polygons.get(n).flatMap(_._2)
}

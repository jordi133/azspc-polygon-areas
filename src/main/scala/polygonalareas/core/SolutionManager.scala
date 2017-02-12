package polygonalareas.core

import java.io.PrintWriter

import polygonalareas._
import polygonalareas.genetic.Polygon

import scala.io.Source
import scala.util.Try

/**
  * Created by Jordi on 18-12-2016.
  *
  * Object keeps track of solutions, saving candidate solutions if they are better than what has been found so far
  */
object SolutionManager {
  val filename = "solutions.txt"
  val bestScoresFilename = "best-raw-scores.txt"

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
    println(s"Best scores:\n${result.map { case (k, v) => (k, doubleSurface(v._1.get), doubleSurface(v._2.get)) }.mkString("\n")}")
    result
  }

  val bestRawScores: Map[Int, Int] = {
    val lines = Source.fromFile(bestScoresFilename).getLines()
    val result = for (size <- puzzleSizes) yield size -> (2 * lines.next().toDouble).toInt
    result.toMap
  }

  def opportunities: List[Int] = {
    val currentScores: Map[Int, Int] = puzzleSizes.map { size => size -> (getMaxSolution(size).map(doubleSurface).getOrElse(size*size) - getMinSolution(size).map(doubleSurface).getOrElse(size*size)) }.toMap
    val opportunities: List[(Int, Double)] = (puzzleSizes zip currentScores) map { case (size, score) => size -> currentScores(size).toDouble / bestRawScores(size) }
    println(s"Opportunities:\n${opportunities.sortBy(_._2).mkString("\n")}")
    println(s"Current score: ${opportunities.map(_._2).sum}")
    opportunities.sortBy(_._2).map(_._1)
  }

  def addSolution(polygon: Seq[Point]) = {
    val size = polygon.size
    getMaxSolution(size) match {
      case Some(max) if doubleSurface(max) > doubleSurface(polygon) =>
      case _ =>
        polygons = polygons.updated(size, (getMinSolution(size), Some(polygon)))
        println(s"Updated max solution for size=$size: ${doubleSurface(polygon) / 2}")
        saveToFile()
    }
    getMinSolution(size) match {
      case Some(min) if doubleSurface(min) < doubleSurface(polygon) =>
      case _ =>
        polygons = polygons.updated(size, (Some(polygon), getMaxSolution(size)))
        println(s"Updated min solution for size=$size: ${doubleSurface(polygon) / 2}")
        saveToFile()
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
      coordinates.toSeq
    }
  }

  def getMinSolution(n: Int): Option[Seq[Point]] = polygons.get(n).flatMap(_._1)

  def getMaxSolution(n: Int): Option[Seq[Point]] = polygons.get(n).flatMap(_._2)
}

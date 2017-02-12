package polygonalareas.gui

import polygonalareas.Point
import polygonalareas.core.SolutionManager
import polygonalareas.generators.{PointGenerator, PolygonGenerator}
import polygonalareas.genetic.Optimizer

import scala.concurrent.Future
import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, Try}
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Rectangle, Polygon => FXPolygon}
import scalafx.stage.Screen

/**
  * Created by Jordi on 11-2-2017.
  *
  * TODO: add search logic and show polygon on next found action
  */
object GuiSearch extends JFXApp {

  var selectedSize = 10

  var running = false

  var polygonScene = new Scene {
    content = new Rectangle {
      x = 25
      y = 40
      width = 100
      height = 100
      fill <== when(hover) choose Green otherwise Red
    }
    onMouseClicked = { _: MouseEvent =>
      if (!running) {
        running = true
        Future(startSearch())
      }
    }
  }

  def startSearch() = {
    val actionOnFoundMax: IndexedSeq[Point] => Unit = { points =>
      SolutionManager.addSolution(points)
    }
    val actionOnFoundMin: IndexedSeq[Point] => Unit = { points =>
      SolutionManager.addSolution(points)
    }
    def actionWithBest(maximize: Boolean): IndexedSeq[Point] => Unit = { points =>
      refreshPolygons(points, maximize)
    }
    def rollExponential(d: Double = Random.nextDouble(), acc: Int = 0): Int = {
      if (d > 0.5) acc
      else rollExponential(2 * d, acc + 1)
    }

    def pg1: Int => Set[Point] = n => PointGenerator.generateRandomPoints()(n)
    def pg2: Int => Set[Point] = n => PointGenerator.generateCrossPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
    def pg3: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
    def pointGenerator: Int => Set[Point] = n => PointGenerator.combine(pg2, pg3)(n)
    //      def polygonGeneratorMin = PolygonGenerator.generateReverseStarPolygon(pointGenerator)
//          def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)
    def polygonGeneratorMin = PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(pointGenerator)
    def polygonGeneratorMax = PolygonGenerator.generateTwoPolygonsInSquare(polygonGeneratorMin)
    //      def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(polygonGeneratorMin)
    for (i <- 1 to 1000) {
      val sizes = SolutionManager.opportunities.filter(_ < 150)
      val n = sizes(Math.min(rollExponential(), sizes.length - 1))
      val optimizer = new Optimizer(
        maxRoundsWithoutImprovement = 4 * (50 - Math.sqrt(n)).toInt,
        familyRevitalizations = 0,
        familySize = 15,
        nrOfFamilies = 1,
        maxOffSpring = 25
      )
      optimizer.optimizeFromPolygon(polygonGeneratorMax, n, true)(actionOnFoundMax, actionWithBest(true))
      optimizer.optimizeFromPolygon(polygonGeneratorMin, n, false)(actionOnFoundMin, actionWithBest(false))

    }

  }

  val screen = Screen.primary
  val bounds = screen.visualBounds

  stage = new JFXApp.PrimaryStage {
    title.value = "Polygons"
    scene = polygonScene
  }

  def pointGenerator: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)

  def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))

  def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)

  def refreshPolygons(pointsLeft: IndexedSeq[Point], maximize: Boolean): Unit = {
    val pointsRight =
      if (maximize) SolutionManager.getMaxSolution(pointsLeft.size).get
      else SolutionManager.getMinSolution(pointsLeft.size).get
    val scale = Math.min(0.5 * polygonScene.getWidth, polygonScene.getHeight) / pointsLeft.size
    val offset = Math.min(0.5 * polygonScene.getWidth, polygonScene.getHeight)
    def polygonPoints(points: Seq[Point], left: Boolean): Seq[Double] =
      if (left) points.flatMap(p => Seq(p.x.toDouble * scale, polygonScene.getHeight - p.y.toDouble * scale))
      else points.flatMap(p => Seq(offset + p.x * scale, polygonScene.getHeight - p.y * scale))

    val left = FXPolygon(polygonPoints(pointsLeft, left = true): _*)
    val right = FXPolygon(polygonPoints(pointsRight, left = false): _*)
    left.fill = Black
    right.fill = Black
    Platform.runLater( polygonScene.content = Seq(left, right))
  }
}
package polygonalareas.gui

import polygonalareas._
import polygonalareas.core.SolutionManager
import polygonalareas.generators.{PointGenerator, PolygonGenerator}
import polygonalareas.genetic.{Optimizer, Polygon}

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

  val screen = Screen.primary
  val bounds = screen.visualBounds

  //  var running = false

  val polygonScene = new Scene {
    //    onMouseClicked = { _: MouseEvent =>
    //      if (!running) {
    //        running = true
    //        Future(startSearch())
    //      }
    //    }
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "Polygons"
    scene = polygonScene
  }

  Future(startSearch())

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
    def pg4: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints(n / 3)(n)
    def pointGenerator: Int => Set[Point] = n =>
      if (Random.nextDouble() <= 0.33) PointGenerator.combine(pg2, pg3)(n)
      else if (Random.nextDouble() < 0.5) pg3(n)
      else pg4(n)
    def polygonGen =
      if (Random.nextDouble() <= 0.25) PolygonGenerator.generateDoubleWedgePolygon(pointGenerator)
      else if (Random.nextDouble() <= 0.33) PolygonGenerator.triangleBasedGeneratorSqrPeripheryBased(pointGenerator) // ok-ish
      else if (Random.nextDouble() <= 0.5) PolygonGenerator.triangleBasedGeneratorSurfaceBased(pointGenerator)
      else PolygonGenerator.generateReverseStarPolygon(pointGenerator)
    def polygonGeneratorMin = polygonGen
    def polygonGeneratorMax =
      if (Random.nextBoolean()) PolygonGenerator.generateTwoOppositePolygonsInSquare(polygonGeneratorMin)
      else if (Random.nextBoolean()  ) PolygonGenerator.generatePolygonInSquare(polygonGeneratorMin)
      else PolygonGenerator.generateTwoPolygonsInSquare(polygonGeneratorMin)
    for (i <- 1 to 1000000) {
      val sizes = SolutionManager.opportunities //.filter(_ < 150)
      val n = sizes.head //(Math.min(rollExponential(), sizes.length - 1))
      val optimizer = new Optimizer(
        maxRoundsWithoutImprovement = 8, //(30 - Math.sqrt(n)).toInt,
        familyRevitalizations = 0,
        familySize = 3,
        nrOfFamilies = 1,
        maxOffSpring = 10,
        generationSteps = 2
      )
      Try(optimizer.optimizeFromPolygonGenerator(polygonGeneratorMax, n, true)(actionOnFoundMax, actionWithBest(true)))
      Try(optimizer.optimizeFromPolygonGenerator(polygonGeneratorMin, n, false)(actionOnFoundMin, actionWithBest(false)))

    }
  }

  def continueFromBestFoundSoFar() = {
    for (n <- puzzleSizes.drop(2)) {
      //        val n = puzzleSizes.last
      val actionOnFound: IndexedSeq[Point] => Unit = { points => SolutionManager.addSolution(points) }
      def actionWithBest(maximize: Boolean): IndexedSeq[Point] => Unit = { points =>
        refreshPolygons(points, maximize)
      }
      val optimizer = new Optimizer(
        maxRoundsWithoutImprovement = 10,
        familyRevitalizations = 0,
        familySize = 3,
        nrOfFamilies = 1,
        maxOffSpring = 25 - Math.sqrt(n).toInt
      )
      SolutionManager.getMaxSolution(n).foreach { polygon =>
        Try(optimizer.optimiseFromPolygon(Polygon(polygon.toIndexedSeq), true)(actionOnFound, actionWithBest(true)))
      }
      SolutionManager.getMinSolution(n).foreach { polygon =>
        Try(optimizer.optimiseFromPolygon(Polygon(polygon.toIndexedSeq), false)(actionOnFound, actionWithBest(false)))
      }
    }
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
    Platform.runLater(polygonScene.content = Seq(left, right))
  }
}
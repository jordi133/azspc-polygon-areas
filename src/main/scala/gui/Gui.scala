package gui

/**
  * Created by Jordi on 18-12-2016.
  */

import polygonalareas._

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Rectangle, Polygon => FXPolygon}
import scalafx.stage.Screen

object Gui extends JFXApp {

  var selectedSize = 22

  var polygonScene = new Scene {
    content = new Rectangle {
      x = 25
      y = 40
      width = 100
      height = 100
      fill <== when(hover) choose Green otherwise Red
    }
    onMouseClicked = { _: MouseEvent =>
      selectedSize = (selectedSize + 1) % puzzleSizes.length
      refreshPolygonOfSize(selectedSize)
      println(s"Showing polygons for n=${puzzleSizes(selectedSize)}")
    }
  }

  val screen = Screen.primary
  val bounds = screen.visualBounds

  stage = new JFXApp.PrimaryStage {
    title.value = "Polygons"
    scene = polygonScene
  }

  val actionOnFound: IndexedSeq[Point] => Unit = { points =>
    SolutionManager.addSolution(points)
    refreshPolygonOfSize(points.size)
  }

  def refreshPolygonOfSize(i: Int): Unit = {
    val pointsLeft = Vector(Point(3, 4), Point(2, 40), Point(1, 46), Point(47, 47), Point(46, 1), Point(20, 2), Point(7, 5), Point(12, 11), Point(11, 6), Point(22, 18), Point(14, 7), Point(23, 19), Point(24, 17), Point(32, 16), Point(33, 27), Point(35, 30), Point(44, 37), Point(40, 3), Point(45, 41), Point(36, 45), Point(42, 44), Point(43, 43), Point(41, 42), Point(25, 39), Point(39, 38), Point(34, 36), Point(38, 35), Point(28, 34), Point(18, 31), Point(37, 33), Point(29, 32), Point(31, 29), Point(30, 28), Point(27, 26), Point(16, 24), Point(4, 25), Point(21, 23), Point(26, 22), Point(17, 21), Point(8, 13), Point(19, 20), Point(15, 15), Point(10, 14), Point(13, 12), Point(5, 10), Point(9, 8), Point(6, 9))
//    val pointsLeft = SolutionManager.getMinSolution(puzzleSizes(selectedSize)).get
    val pointsRight = SolutionManager.getMaxSolution(puzzleSizes(selectedSize)).get
    val scale = Math.min(0.5 * polygonScene.getWidth, polygonScene.getHeight) / pointsLeft.size
    val offset = Math.min(0.5 * polygonScene.getWidth, polygonScene.getHeight)
    def polygonPoints(points: Seq[Point], left: Boolean): Seq[Double] =
      if (left) points.flatMap(p => Seq(p.x.toDouble * scale, p.y.toDouble * scale))
      else points.flatMap(p => Seq(offset + p.x * scale, p.y * scale))

    val left = FXPolygon(polygonPoints(pointsLeft, left = true): _*)
    val right = FXPolygon(polygonPoints(pointsRight, left = false): _*)
    polygonScene.content.clear()
    left.fill = Black
    right.fill = Black
    polygonScene.content = Seq(left, right)
  }

  refreshPolygonOfSize(selectedSize)

}
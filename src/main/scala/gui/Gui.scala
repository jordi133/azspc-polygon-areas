package gui

/**
  * Created by Jordi on 18-12-2016.
  */

import polygonalareas._

import scala.util.Try
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Rectangle, Polygon => FXPolygon}
import scalafx.stage.Screen

object Gui extends JFXApp {

  var selectedSize = 0


  var polygonScene = new Scene {
    content = new Rectangle {
      x = 25
      y = 40
      width = 100
      height = 100
      fill <== when(hover) choose Green otherwise Red
    }
    onMouseClicked = {_: MouseEvent =>
      selectedSize = (selectedSize + 1) % puzzleSizes.length
      refreshPolygonOfSize(selectedSize)
    }
  }

  val screen = Screen.primary
  val bounds = screen.visualBounds

  stage = new JFXApp.PrimaryStage {
//    x = bounds.minX
//    y = bounds.minY
//    width = bounds.width
//    height = bounds.height
    title.value = "Hello Stage"
    scene = polygonScene
  }

  val actionOnFound: IndexedSeq[Point] => Unit = { points =>
    SolutionManager.addSolution(points)
    refreshPolygonOfSize(points.size)
  }

  def refreshPolygonOfSize(i: Int): Unit = {
    val pointsLeft = SolutionManager.getMinSolution(puzzleSizes(selectedSize)).get
    val pointsRight = SolutionManager.getMinSolution(puzzleSizes(selectedSize)).get
    val scale = pointsLeft.size / 2 * Math.min(stage.getWidth, stage.getHeight)
    def polygonPoints(points: Seq[Point], left: Boolean): Seq[Double] =
      if (left) points.flatMap(p => Seq(p.x / scale, p.y / scale))
      else points.flatMap(p => Seq(bounds.width / 2 + p.x / scale, p.y / scale))

    val left = FXPolygon(polygonPoints(pointsLeft, left = true): _*)
    val right = FXPolygon(polygonPoints(pointsRight, left = false): _*)
println(s"refreshing polygons")
    polygonScene.content.clear()
    left.fill = Black
    right.fill = Black
    polygonScene.content.add(left)
    polygonScene.content.add(right)
  }

  refreshPolygonOfSize(selectedSize)

}
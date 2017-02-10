package polygonalareas.gui

/**
  * Created by Jordi on 18-12-2016.
  */

import polygonalareas._
import polygonalareas.core.SolutionManager
import polygonalareas.generators.{PointGenerator, PolygonGenerator}

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Rectangle, Polygon => FXPolygon}
import scalafx.stage.Screen

object Gui extends JFXApp {

  var selectedSize = 20

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
  val n = puzzleSizes(4)
  def pointGenerator: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n)
  def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))
  def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)

  def refreshPolygonOfSize(i: Int): Unit = {
//    val pointsLeft = Vector(Point(2,1), Point(16,12), Point(21,4), Point(27,20), Point(38,8), Point(34,24), Point(35,25), Point(45,17), Point(52,9), Point(49,18), Point(41,34), Point(56,15), Point(65,3), Point(58,14), Point(44,36), Point(61,11), Point(53,27), Point(62,26), Point(71,19), Point(74,21), Point(67,31), Point(85,13), Point(91,7), Point(70,32), Point(98,6), Point(72,33), Point(103,5), Point(92,16), Point(86,23), Point(81,28), Point(90,22), Point(75,39), Point(66,49), Point(79,40), Point(77,42), Point(97,37), Point(64,61), Point(104,35), Point(68,64), Point(101,46), Point(78,62), Point(80,63), Point(96,59), Point(93,66), Point(82,72), Point(102,68), Point(83,79), Point(100,75), Point(94,89), Point(99,97), Point(1,2), Point(4,10), Point(3,51), Point(5,54), Point(6,50), Point(11,48), Point(7,77), Point(19,30), Point(20,29), Point(8,76), Point(18,45), Point(15,55), Point(24,38), Point(22,52), Point(29,41), Point(31,43), Point(9,88), Point(32,47), Point(28,57), Point(25,65), Point(12,86), Point(33,53), Point(40,44), Point(30,60), Point(37,56), Point(43,58), Point(14,91), Point(36,70), Point(13,93), Point(26,82), Point(23,85), Point(47,69), Point(51,67), Point(10,98), Point(46,74), Point(42,78), Point(39,81), Point(55,73), Point(69,71), Point(60,80), Point(54,84), Point(48,87), Point(73,83), Point(88,90), Point(59,95), Point(84,92), Point(89,94), Point(87,96), Point(57,100), Point(63,101), Point(95,99), Point(50,102), Point(17,104), Point(76,103), Point(105,105))
//        val pointsLeft = polygonGeneratorMax(n)
//    println(s"pointsLeft size = ${pointsLeft.size}, n=$selectedSize")
//    val pointsRight = polygonGeneratorMin(n)
    val pointsLeft = SolutionManager.getMinSolution(puzzleSizes(selectedSize)).get
    val pointsRight = SolutionManager.getMaxSolution(puzzleSizes(selectedSize)).get
    val scale = Math.min(0.5 * polygonScene.getWidth, polygonScene.getHeight) / pointsLeft.size
    val offset = Math.min(0.5 * polygonScene.getWidth, polygonScene.getHeight)
    def polygonPoints(points: Seq[Point], left: Boolean): Seq[Double] =
      if (left) points.flatMap(p => Seq(p.x.toDouble * scale, polygonScene.getHeight - p.y.toDouble * scale))
      else points.flatMap(p => Seq(offset + p.x * scale,polygonScene.getHeight- p.y * scale))

    val left = FXPolygon(polygonPoints(pointsLeft, left = true): _*)
    val right = FXPolygon(polygonPoints(pointsRight, left = false): _*)
    polygonScene.content.clear()
    left.fill = Black
    right.fill = Black
    polygonScene.content = Seq(left, right)
  }

  refreshPolygonOfSize(selectedSize)

}
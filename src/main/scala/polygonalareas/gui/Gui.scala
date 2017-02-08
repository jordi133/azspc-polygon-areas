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
  def pointGenerator: Int => Set[Point] = n => PointGenerator.generateDiagonalPoints((1.5 * Math.pow(n, 0.5)).toInt)(n).toSet
  def polygonGeneratorMax = PolygonGenerator.generatePolygonInSquare(PolygonGenerator.generateWedgePolygon(pointGenerator))
  def polygonGeneratorMin = PolygonGenerator.generateWedgePolygon(pointGenerator)

  def refreshPolygonOfSize(i: Int): Unit = {
    val pointsLeft = Vector(Point(54,54), Point(58,46), Point(62,60), Point(64,64), Point(46,56), Point(31,89), Point(27,81), Point(25,77), Point(29,99), Point(19,95), Point(33,103), Point(37,91), Point(43,93), Point(51,85), Point(47,69), Point(75,105), Point(55,79), Point(50,62), Point(66,72), Point(72,80), Point(90,102), Point(94,100), Point(88,98), Point(70,76), Point(92,96), Point(84,88), Point(99,101), Point(78,82), Point(98,94), Point(101,97), Point(104,104), Point(100,92), Point(81,83), Point(102,90), Point(96,86), Point(86,84), Point(74,78), Point(48,58), Point(68,66), Point(82,74), Point(87,73), Point(77,71), Point(105,53), Point(103,51), Point(80,68), Point(83,59), Point(93,41), Point(91,55), Point(97,31), Point(76,70), Point(71,49), Point(95,7), Point(85,3), Point(89,15), Point(73,25), Point(67,21), Point(69,29), Point(79,23), Point(65,63), Point(59,17), Point(61,33), Point(63,57), Point(60,50), Point(53,9), Point(57,37), Point(49,19), Point(56,44), Point(40,42), Point(38,34), Point(45,39), Point(36,32), Point(41,45), Point(32,28), Point(39,27), Point(42,24), Point(35,5), Point(30,26), Point(22,2), Point(20,10), Point(24,30), Point(16,4), Point(17,11), Point(11,1), Point(14,8), Point(18,22), Point(10,14), Point(6,6), Point(4,12), Point(2,20), Point(1,65), Point(7,67), Point(5,47), Point(15,61), Point(13,35), Point(8,16), Point(23,43), Point(12,18), Point(26,40), Point(3,87), Point(28,36), Point(21,13), Point(34,38), Point(44,52), Point(9,75), Point(52,48))
    //    val pointsLeft = polygonGeneratorMax(n)
//    println(s"pointsLeft size = ${pointsLeft.size}, n=$selectedSize")
    val pointsRight = polygonGeneratorMin(n)
//    val pointsLeft = SolutionManager.getMinSolution(puzzleSizes(selectedSize)).get
//    val pointsRight = SolutionManager.getMaxSolution(puzzleSizes(selectedSize)).get
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
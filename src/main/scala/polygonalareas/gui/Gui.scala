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

  var selectedSize = 10

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
//    val pointsLeft = Vector(Point(3,1), Point(97,3), Point(61,4), Point(95,5), Point(51,6), Point(67,7), Point(81,8), Point(77,10), Point(43,11), Point(63,12), Point(91,13), Point(47,14), Point(89,17), Point(65,18), Point(73,19), Point(85,21), Point(59,23), Point(93,25), Point(79,26), Point(83,30), Point(71,31), Point(75,33), Point(99,44), Point(101,51), Point(103,53), Point(102,54), Point(26,56), Point(54,57), Point(18,58), Point(88,59), Point(60,60), Point(34,61), Point(22,62), Point(38,63), Point(40,64), Point(68,65), Point(66,66), Point(4,70), Point(52,72), Point(50,74), Point(14,75), Point(20,76), Point(30,77), Point(16,79), Point(48,80), Point(44,81), Point(42,83), Point(8,92), Point(6,93), Point(12,99), Point(10,100), Point(2,105), Point(24,101), Point(28,102), Point(32,98), Point(36,103), Point(46,88), Point(56,91), Point(58,78), Point(62,90), Point(64,89), Point(70,97), Point(72,71), Point(74,84), Point(76,94), Point(78,86), Point(80,67), Point(82,96), Point(84,69), Point(86,95), Point(90,73), Point(92,85), Point(94,104), Point(96,87), Point(98,82), Point(100,68), Point(104,55), Point(105,52), Point(87,45), Point(69,41), Point(57,48), Point(55,50), Point(53,39), Point(49,46), Point(45,36), Point(41,38), Point(39,35), Point(37,28), Point(35,32), Point(33,20), Point(31,37), Point(29,42), Point(27,15), Point(25,16), Point(23,47), Point(21,22), Point(19,27), Point(17,34), Point(15,43), Point(13,29), Point(11,9), Point(9,24), Point(7,49), Point(5,40), Point(1,2))
        val pointsLeft = SolutionManager.getMinSolution(puzzleSizes(selectedSize)).get
//    val pointsRight = Vector(Point(5,17), Point(1,2), Point(2,292), Point(293,293), Point(292,1), Point(11,3), Point(18,8), Point(6,5), Point(12,21), Point(33,13), Point(30,10), Point(26,12), Point(35,4), Point(34,14), Point(21,18), Point(16,30), Point(69,38), Point(22,32), Point(29,41), Point(54,42), Point(64,51), Point(66,65), Point(77,9), Point(67,63), Point(89,80), Point(72,68), Point(85,87), Point(102,84), Point(93,88), Point(100,92), Point(104,77), Point(101,100), Point(122,110), Point(108,107), Point(127,112), Point(134,82), Point(132,127), Point(137,128), Point(153,151), Point(156,140), Point(171,125), Point(165,126), Point(169,117), Point(159,121), Point(163,104), Point(150,79), Point(167,111), Point(196,78), Point(233,62), Point(203,75), Point(237,64), Point(247,55), Point(232,48), Point(250,53), Point(227,40), Point(238,46), Point(258,27), Point(241,47), Point(261,31), Point(257,25), Point(88,73), Point(264,23), Point(259,24), Point(268,22), Point(279,7), Point(277,11), Point(270,20), Point(274,33), Point(287,28), Point(281,54), Point(285,36), Point(275,37), Point(282,57), Point(273,34), Point(267,29), Point(288,93), Point(266,26), Point(253,45), Point(255,50), Point(244,58), Point(251,69), Point(220,89), Point(249,70), Point(228,67), Point(217,71), Point(205,76), Point(192,83), Point(224,96), Point(210,91), Point(216,102), Point(260,74), Point(221,99), Point(248,124), Point(223,101), Point(225,118), Point(212,95), Point(204,90), Point(191,85), Point(164,116), Point(174,109), Point(173,114), Point(184,123), Point(175,119), Point(168,130), Point(176,129), Point(189,131), Point(186,105), Point(190,135), Point(181,134), Point(166,132), Point(155,146), Point(177,152), Point(154,148), Point(162,158), Point(178,160), Point(185,163), Point(245,144), Point(179,165), Point(230,184), Point(183,167), Point(172,172), Point(182,183), Point(195,197), Point(209,208), Point(202,200), Point(218,217), Point(214,191), Point(215,212), Point(213,181), Point(219,224), Point(242,226), Point(234,240), Point(240,230), Point(246,231), Point(231,244), Point(239,247), Point(256,243), Point(254,265), Point(263,282), Point(272,266), Point(269,273), Point(283,258), Point(280,207), Point(284,268), Point(290,287), Point(289,19), Point(291,290), Point(286,281), Point(278,267), Point(271,277), Point(276,285), Point(265,284), Point(194,270), Point(262,283), Point(243,253), Point(200,271), Point(252,249), Point(222,246), Point(199,261), Point(235,237), Point(197,242), Point(226,238), Point(236,235), Point(229,229), Point(207,222), Point(193,248), Point(208,219), Point(188,220), Point(206,218), Point(211,211), Point(198,202), Point(201,209), Point(187,189), Point(170,174), Point(158,187), Point(161,177), Point(180,168), Point(160,159), Point(157,155), Point(151,171), Point(148,227), Point(152,150), Point(147,178), Point(149,289), Point(146,162), Point(144,143), Point(140,251), Point(143,147), Point(138,173), Point(139,164), Point(133,166), Point(131,192), Point(130,170), Point(124,257), Point(129,175), Point(119,188), Point(112,186), Point(117,199), Point(110,182), Point(96,263), Point(106,205), Point(105,190), Point(90,204), Point(91,213), Point(99,210), Point(86,216), Point(94,223), Point(82,215), Point(81,203), Point(68,232), Point(62,239), Point(70,252), Point(61,241), Point(50,250), Point(37,286), Point(48,254), Point(39,264), Point(40,269), Point(27,279), Point(31,274), Point(24,280), Point(23,276), Point(7,291), Point(13,259), Point(9,288), Point(20,278), Point(17,255), Point(3,194), Point(19,262), Point(25,275), Point(36,272), Point(38,260), Point(45,233), Point(41,256), Point(56,245), Point(57,228), Point(43,180), Point(59,234), Point(65,221), Point(63,236), Point(71,225), Point(76,214), Point(75,201), Point(74,198), Point(87,206), Point(83,195), Point(97,196), Point(114,157), Point(73,179), Point(115,156), Point(118,169), Point(121,185), Point(126,176), Point(141,153), Point(135,161), Point(142,149), Point(136,154), Point(145,141), Point(125,122), Point(123,138), Point(109,142), Point(116,137), Point(111,133), Point(120,136), Point(128,113), Point(107,108), Point(113,120), Point(103,103), Point(98,97), Point(84,145), Point(95,106), Point(92,86), Point(79,94), Point(55,193), Point(78,98), Point(80,81), Point(58,56), Point(51,72), Point(60,49), Point(52,43), Point(49,52), Point(47,59), Point(53,66), Point(42,61), Point(46,60), Point(44,44), Point(15,39), Point(28,115), Point(32,139), Point(14,35), Point(10,16), Point(8,15), Point(4,6))
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
package gui

/**
  * Created by Jordi on 18-12-2016.
  */

import polygonalareas._
import polygonalareas.genetic.PointGenerator
import polygonalareas.genetic.PolygonGenerator

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
    val pointsLeft = Vector(Point(3,4), Point(2,2), Point(1,466), Point(467,467), Point(466,1), Point(4,3), Point(120,202), Point(122,180), Point(123,183), Point(125,209), Point(126,195), Point(128,186), Point(129,192), Point(133,217), Point(127,160), Point(121,127), Point(139,214), Point(134,187), Point(136,194), Point(135,185), Point(130,161), Point(140,188), Point(131,157), Point(155,219), Point(146,190), Point(150,200), Point(141,174), Point(152,201), Point(147,184), Point(164,210), Point(163,206), Point(138,155), Point(143,163), Point(232,325), Point(151,177), Point(237,326), Point(226,306), Point(176,218), Point(148,169), Point(149,170), Point(224,296), Point(223,294), Point(238,313), Point(240,314), Point(231,297), Point(174,205), Point(249,322), Point(177,207), Point(225,279), Point(167,191), Point(229,283), Point(247,309), Point(251,311), Point(124,126), Point(259,320), Point(221,265), Point(228,273), Point(261,318), Point(332,411), Point(327,403), Point(252,300), Point(255,304), Point(347,428), Point(336,412), Point(266,316), Point(258,305), Point(256,302), Point(230,266), Point(166,181), Point(339,409), Point(257,299), Point(233,267), Point(181,199), Point(253,292), Point(338,401), Point(185,203), Point(350,410), Point(353,407), Point(367,424), Point(282,319), Point(375,433), Point(328,375), Point(364,419), Point(348,399), Point(344,394), Point(274,308), Point(330,376), Point(335,382), Point(358,405), Point(145,150), Point(222,242), Point(340,381), Point(260,286), Point(359,402), Point(198,212), Point(227,246), Point(346,386), Point(373,417), Point(370,413), Point(357,396), Point(334,368), Point(158,164), Point(235,252), Point(241,258), Point(284,307), Point(246,263), Point(395,432), Point(300,323), Point(248,264), Point(276,295), Point(161,166), Point(389,421), Point(356,384), Point(341,365), Point(268,284), Point(186,193), Point(196,204), Point(272,287), Point(398,425), Point(267,281), Point(345,366), Point(263,276), Point(288,303), Point(402,427), Point(351,371), Point(403,426), Point(264,275), Point(388,408), Point(297,310), Point(410,431), Point(369,387), Point(371,389), Point(302,315), Point(308,321), Point(236,244), Point(440,462), Point(444,465), Point(262,271), Point(329,342), Point(331,344), Point(333,346), Point(337,350), Point(365,379), Point(280,289), Point(361,374), Point(439,456), Point(438,454), Point(379,392), Point(445,461), Point(391,404), Point(449,464), Point(275,282), Point(234,239), Point(443,457), Point(242,247), Point(446,459), Point(220,224), Point(387,397), Point(205,208), Point(355,363), Point(269,274), Point(273,278), Point(244,248), Point(377,385), Point(414,423), Point(318,324), Point(412,420), Point(363,369), Point(250,253), Point(437,444), Point(448,455), Point(214,216), Point(411,416), Point(425,430), Point(453,458), Point(458,463), Point(418,422), Point(436,440), Point(299,301), Point(426,429), Point(447,450), Point(381,383), Point(413,414), Point(459,460), Point(168,168), Point(243,243), Point(434,434), Point(435,435), Point(416,415), Point(360,359), Point(313,312), Point(455,453), Point(441,439), Point(271,270), Point(408,406), Point(349,347), Point(454,451), Point(450,446), Point(442,437), Point(385,380), Point(215,213), Point(399,393), Point(343,338), Point(460,452), Point(451,443), Point(362,356), Point(323,317), Point(457,447), Point(342,335), Point(404,395), Point(428,418), Point(304,298), Point(456,445), Point(386,377), Point(199,196), Point(409,398), Point(461,448), Point(245,240), Point(384,373), Point(383,372), Point(452,438), Point(465,449), Point(354,343), Point(392,378), Point(462,442), Point(203,198), Point(301,290), Point(368,352), Point(420,400), Point(464,441), Point(279,268), Point(366,348), Point(380,360), Point(171,167), Point(463,436), Point(352,333), Point(417,391), Point(162,158), Point(382,357), Point(303,285), Point(394,367), Point(310,291), Point(421,390), Point(265,250), Point(157,153), Point(156,152), Point(277,259), Point(239,225), Point(397,364), Point(427,388), Point(319,293), Point(254,236), Point(376,341), Point(401,361), Point(316,288), Point(189,179), Point(390,351), Point(415,370), Point(407,362), Point(378,337), Point(374,332), Point(154,148), Point(372,328), Point(285,256), Point(315,280), Point(406,354), Point(281,251), Point(314,277), Point(296,261), Point(396,340), Point(307,269), Point(289,254), Point(291,255), Point(298,260), Point(419,353), Point(219,197), Point(422,355), Point(429,358), Point(393,330), Point(190,173), Point(400,331), Point(424,349), Point(292,249), Point(278,238), Point(324,272), Point(423,345), Point(270,229), Point(405,327), Point(192,172), Point(170,156), Point(294,245), Point(217,189), Point(202,178), Point(433,339), Point(286,235), Point(432,336), Point(326,262), Point(200,175), Point(290,237), Point(431,334), Point(287,234), Point(321,257), Point(430,329), Point(283,228), Point(293,230), Point(295,231), Point(312,241), Point(210,176), Point(309,233), Point(306,226), Point(305,222), Point(153,138), Point(144,133), Point(325,232), Point(311,223), Point(320,227), Point(169,145), Point(208,165), Point(317,221), Point(322,220), Point(206,162), Point(191,154), Point(132,125), Point(173,143), Point(180,146), Point(218,159), Point(183,144), Point(209,151), Point(178,139), Point(204,147), Point(193,141), Point(159,131), Point(172,134), Point(201,142), Point(197,140), Point(184,135), Point(195,136), Point(213,137), Point(175,128), Point(182,129), Point(216,132), Point(211,130), Point(142,122), Point(165,123), Point(194,124), Point(179,121), Point(212,120), Point(207,119), Point(187,117), Point(188,116), Point(160,113), Point(137,115), Point(111,29), Point(109,14), Point(107,6), Point(105,24), Point(104,20), Point(106,45), Point(102,49), Point(110,82), Point(112,91), Point(88,5), Point(99,47), Point(91,28), Point(108,85), Point(85,18), Point(96,60), Point(92,50), Point(90,48), Point(93,56), Point(103,80), Point(81,31), Point(82,40), Point(68,12), Point(65,9), Point(87,54), Point(94,70), Point(73,32), Point(75,42), Point(80,51), Point(70,35), Point(62,22), Point(63,27), Point(52,10), Point(100,88), Point(116,114), Point(57,25), Point(66,39), Point(44,7), Point(47,16), Point(79,64), Point(76,61), Point(39,15), Point(41,21), Point(30,8), Point(78,68), Point(59,46), Point(27,11), Point(49,37), Point(31,19), Point(38,30), Point(64,59), Point(95,93), Point(42,36), Point(40,34), Point(17,13), Point(29,26), Point(98,98), Point(77,77), Point(16,17), Point(22,23), Point(101,102), Point(53,58), Point(61,67), Point(33,43), Point(58,66), Point(28,41), Point(18,33), Point(35,57), Point(8,38), Point(74,87), Point(69,84), Point(11,44), Point(36,62), Point(37,65), Point(21,55), Point(97,105), Point(50,76), Point(12,53), Point(26,63), Point(55,81), Point(5,52), Point(86,100), Point(67,89), Point(45,79), Point(72,94), Point(34,74), Point(56,86), Point(19,69), Point(84,103), Point(23,75), Point(13,72), Point(10,71), Point(9,73), Point(20,78), Point(43,92), Point(14,83), Point(46,95), Point(51,97), Point(89,110), Point(6,90), Point(71,107), Point(113,118), Point(15,96), Point(83,112), Point(24,101), Point(60,108), Point(48,106), Point(7,99), Point(25,104), Point(54,111), Point(32,109), Point(115,149), Point(114,171), Point(117,182), Point(118,211), Point(119,215))
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
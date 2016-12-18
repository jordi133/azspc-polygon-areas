package gui

/**
  * Created by Jordi on 18-12-2016.
  */

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Rectangle, Polygon => FXPolygon}

object Gui extends JFXApp {

  var polygonScene = new Scene {
    fill = LightGreen
    content = Seq(
      new Rectangle {
        x = 25
        y = 40
        width = 100
        height = 100
        fill <== when(hover) choose Green otherwise Red
      },
      FXPolygon(40,0,50,20,30,40,20,30,10,50,0,10)
    )
  }


  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = 600
    height = 450
    scene = polygonScene
  }

}
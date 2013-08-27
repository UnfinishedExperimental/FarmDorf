package de.dheinrich.farmer.gui

import java.net.URL
import java.util.ResourceBundle
import javafx.scene.input.MouseEvent
import javafx.scene.{ control => jfxc }
import javafx.{ fxml => jfxf }
import javafx.{ scene => jfxs }
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.Platform
import scalafx.beans.property._
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.control.ListView
import scalafx.scene.control.SelectionMode
import scalafx.scene.control.cell.TextFieldListCell
import scalafx.util.StringConverter
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.image.Image
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.HBox
import scalafx.scene.text.Text
import scalafx.scene.paint.LinearGradient
import scalafx.scene.paint.Stops
import scalafx.scene.effect.DropShadow
import scalafx.scene.effect.Reflection
import scalafx.scene.effect.Lighting
import scalafx.scene.effect.Light
import scalafx.application.ConditionalFeature
import scalafx.scene.canvas.Canvas
import scalafx.animation.Timeline
import scalafx.scene.canvas.GraphicsContext
import scalafx.animation.KeyFrame
import scala.math.random
import scala.collection.immutable.VectorBuilder
import scalafx.scene.Group
import scalafx.scene.shape.Circle
import scalafx.scene.paint.CycleMethod._
import scalafx.scene.shape.StrokeType._
import scalafx.scene.effect.BoxBlur
import scalafx.animation.Timeline._
import scalafx.scene.effect.BlendMode._

object MapTest extends JFXApp {
//  val WIDTH = 800
//  val HEIGHT = 600
//
//  stage = new JFXApp.PrimaryStage {
//    title = "Hello Stage"
//    width = WIDTH
//    height = HEIGHT
//    resizable = false
//
//    scene = new Scene {
//      fill = LIGHTGREEN
//      content = Seq(
//        new Rectangle {
//          width <== scene.width
//          height <== scene.height
//          fill = BLACK
//        })
//    }
//  }
  val circlesToAnimate = new VectorBuilder[Circle]()
  stage = new PrimaryStage {
    width = 800
    height = 600
    scene = new Scene {
      fill = BLACK
      content = Seq(
        new Group {
          children = Seq(
            new Rectangle {
              width <== scene.width
              height <== scene.height
              fill = BLACK
            },
            new Group {
              val circles = for (i <- 0 until 15) yield new Circle {
                radius = 200
                fill = WHITE opacity 0.05
                stroke = WHITE opacity 0.2
                strokeWidth = 4
                strokeType = OUTSIDE
              }
              children = circles
              circlesToAnimate ++= circles
              effect = new BoxBlur(30, 30, 3)
            },
            new Group {
              val circles = for (i <- 0 until 20) yield new Circle {
                radius = 70
                fill = WHITE opacity 0.05
                stroke = WHITE opacity 0.1
                strokeWidth = 2
                strokeType = OUTSIDE
              }
              children = circles
              circlesToAnimate ++= circles
              effect = new BoxBlur(2, 2, 2)
            },
            new Group {
              val circles = for (i <- 0 until 10) yield new Circle {
                radius = 150
                fill = WHITE opacity 0.05
                stroke = WHITE opacity 0.16
                strokeWidth = 4
                strokeType = OUTSIDE
              }
              children = circles
              circlesToAnimate ++= circles
              effect = new BoxBlur(10, 10, 3)
            })
        },
        new Rectangle {
          width <== scene.width
          height <== scene.height
          fill = new LinearGradient(0, 1, 1, 0, true, NO_CYCLE,
            Stops(0xf8bd55, 0xc0fe56, 0x5dfbc1, 0x64c2f8, 0xbe4af7, 0xed5fc2, 0xef504c, 0xf2660f))
          blendMode = OVERLAY
        }
      )
    }
  }
  new Timeline {
    cycleCount = INDEFINITE
    autoReverse = true
    keyFrames = (for (circle <- circlesToAnimate.result()) yield Seq(
      at(0 s) {Set(circle.centerX -> random * 800,
                   circle.centerY -> random * 600)},
      at(40 s) {Set(circle.centerX -> random * 800,
                    circle.centerY -> random * 600)}
    )).flatten
  }.play()
}
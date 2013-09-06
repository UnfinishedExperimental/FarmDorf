//package de.dheinrich.farmer.gui
//
//import java.net.URL
//import java.util.ResourceBundle
//import javafx.scene.input.MouseEvent
//import javafx.scene.{ control => jfxc }
//import javafx.{ fxml => jfxf }
//import javafx.{ scene => jfxs }
//import scalafx.Includes._
//import scalafx.application.JFXApp
//import scalafx.application.JFXApp.PrimaryStage
//import scalafx.application.Platform
//import scalafx.beans.property._
//import scalafx.collections.ObservableBuffer
//import scalafx.scene.Scene
//import scalafx.scene.control.ListView
//import scalafx.scene.control.SelectionMode
//import scalafx.scene.control.cell.TextFieldListCell
//import scalafx.util.StringConverter
//import scalafx.scene.paint.Color._
//import scalafx.scene.shape.Rectangle
//import scalafx.scene.image.Image
//import scalafx.scene.layout.StackPane
//import scalafx.scene.layout.HBox
//import scalafx.scene.text.Text
//import scalafx.scene.paint.LinearGradient
//import scalafx.scene.paint.Stops
//import scalafx.scene.effect.DropShadow
//import scalafx.scene.effect.Reflection
//import scalafx.scene.effect.Lighting
//import scalafx.scene.effect.Light
//import scalafx.application.ConditionalFeature
//import scalafx.scene.canvas.Canvas
//import scalafx.animation.Timeline
//import scalafx.scene.canvas.GraphicsContext
//import scalafx.animation.KeyFrame
//import scala.math.random
//import scala.collection.immutable.VectorBuilder
//import scalafx.scene.Group
//import scalafx.scene.shape.Circle
//import scalafx.scene.paint.CycleMethod._
//import scalafx.scene.shape.StrokeType._
//import scalafx.scene.effect.BoxBlur
//import scalafx.animation.Timeline._
//import scalafx.scene.effect.BlendMode._
//
//object MapTest extends JFXApp {
//  //  val WIDTH = 800
//  //  val HEIGHT = 600
//  //
//  //  stage = new JFXApp.PrimaryStage {
//  //    title = "Hello Stage"
//  //    width = WIDTH
//  //    height = HEIGHT
//  //    resizable = false
//  //
//  //    scene = new Scene {
//  //      fill = LIGHTGREEN
//  //      content = Seq(
//  //        new Rectangle {
//  //          width <== scene.width
//  //          height <== scene.height
//  //          fill = BLACK
//  //        })
//  //    }
//  //  }
//  val circlesToAnimate = new VectorBuilder[Circle]()
//  stage = new PrimaryStage {
//    width = 800
//    height = 600
//    scene = new Scene {
//      fill = BLACK
//      content = new TableView() {
//
//      }
//    }
//  }
//}
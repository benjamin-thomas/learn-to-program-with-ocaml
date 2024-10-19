//> using dep "org.scalafx::scalafx:22.0.0-R33"

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.application.HostServices

sealed trait Quad
case object White extends Quad
case object Black extends Quad
case class Node(q1: Quad, q2: Quad, q3: Quad, q4: Quad) extends Quad

/*

Closer to the Haskell version, than the OCaml version.

scala-cli compile --watch ./Main.scala
scala-cli ./Main.scala

 */

object App extends JFXApp3 {

  def checkerBoard(n: Int): Quad =
    n match
      case 0 => Black
      case 1 => Node(White, Black, White, Black)
      case n =>
        val quad = checkerBoard(n - 1)
        Node(quad, quad, quad, quad)

  def quadToRectangles(
      qx: Double,
      qy: Double,
      size: Double,
      quad: Quad
  ): List[Rectangle] = quad match {
    case White =>
      List(new Rectangle {
        x = qx
        y = qy
        width = size
        height = size
        fill = Color.White
      })
    case Black =>
      List(new Rectangle {
        x = qx
        y = qy
        width = size
        height = size
        fill = Color.Black
      })
    case Node(q1, q2, q3, q4) =>
      val h = size / 2
      // format: off
      List.concat( quadToRectangles(qx + 0, qy + 0, h, q1)
                 , quadToRectangles(qx + h, qy + 0, h, q2)
                 , quadToRectangles(qx + h, qy + h, h, q3)
                 , quadToRectangles(qx + 0, qy + h, h, q4)
                 )
      // format: on

  }

  def start(): Unit =
    val pow = 3
    val gridSize = math.pow(2, pow).toInt
    val tileSize = 20
    val boardSize = gridSize * tileSize

    stage = new JFXApp3.PrimaryStage {
      title.value = "Checkerboard"
      width = boardSize
      height = boardSize
      scene = new Scene {
        fill = Color.Red
        content = quadToRectangles(0, 0, boardSize, checkerBoard(pow))
      }
    }
}

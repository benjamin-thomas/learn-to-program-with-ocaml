//> using dep "org.processing:core:3.3.7"

import processing.core.PApplet

/*

Similar in spirit to the OCaml version.

scala-cli ./Main.scala --jvm 21

 */

sealed trait Quad
case object White extends Quad
case object Black extends Quad
case class Node(q1: Quad, q2: Quad, q3: Quad, q4: Quad) extends Quad

object ProcessingCheckerboardApp extends PApplet {

  def checkerBoard(n: Int): Quad =
    n match {
      case 0 => Black
      case 1 => Node(White, Black, White, Black)
      case n =>
        val quad = checkerBoard(n - 1)
        Node(quad, quad, quad, quad)
    }

  def drawBoard(x: Float, y: Float, size: Float, quad: Quad): Unit = {
    quad match {
      case White =>
        fill(255)
        rect(x, y, size, size)
      case Black =>
        fill(0)
        rect(x, y, size, size)
      case Node(q1, q2, q3, q4) =>
        val h = size / 2
        drawBoard(x + 0, y + 0, h, q1) // Bottom-left
        drawBoard(x + h, y + 0, h, q2) // Bottom-right
        drawBoard(x + h, y + h, h, q3) // Top-right
        drawBoard(x + 0, y + h, h, q4) // Top-left
    }
  }

  override def settings(): Unit = {
    size(400, 400)
  }

  override def draw(): Unit = {
    background(255)

    val pow = 3
    val gridSize = math.pow(2, pow).toInt
    val tileSize = width / gridSize.toFloat

    val board = checkerBoard(pow)
    drawBoard(0, 0, width.toFloat, board)
  }

  def main(args: Array[String]): Unit = {
    PApplet.runSketch(Array("ProcessingCheckerboardApp"), this)
  }
}

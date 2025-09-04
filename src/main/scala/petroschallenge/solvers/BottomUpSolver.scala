package petroschallenge.solvers

import cats.effect.IO
import petroschallenge.model.Types.TriangleNode
import petroschallenge.model.{MinimumTriangleSolver}

import scala.collection.mutable.ArrayBuffer

object BottomUpSolver extends MinimumTriangleSolver {
  def solveTriangle(
      triangle: Vector[Vector[Int]]
  ): IO[TriangleNode] =
    IO.delay {

      // not happy using mutable objects
      val dinamicTriangle = triangle.map(_.to(ArrayBuffer)).to(ArrayBuffer)

      // Fase 1: down to up getting the sum
      for (row <- dinamicTriangle.length - 2 to 0 by -1) {
        for (column <- 0 until dinamicTriangle(row).length) {
          val currentPosition = dinamicTriangle(row)(column)
          val leftTree        = dinamicTriangle(row + 1)(column)
          val rightTree       = dinamicTriangle(row + 1)(column + 1)
          val minimalSum      = currentPosition + math.min(leftTree, rightTree)
          dinamicTriangle(row)(column) = minimalSum
        }
      }

      // Fase 2: rebuilding the path
      var path          = ArrayBuffer[Int]()
      var currentColumn = 0

      // this is top down
      for (row <- triangle.indices) {
        path += triangle(row)(currentColumn)
        if (row < triangle.length - 1) {
          val valorInferior1 = dinamicTriangle(row + 1)(currentColumn)
          val valorInferior2 = dinamicTriangle(row + 1)(currentColumn + 1)
          if (valorInferior2 < valorInferior1) {
            currentColumn += 1
          }
        }
      }

      val total = dinamicTriangle(0)(0)
      (total, path.toVector)
    }

}

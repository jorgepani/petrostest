package petroschallenge

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => MutableMap}

object TriangleSolver {
  def solveFromBottomToTop(
                            triangle: Vector[Vector[Int]]
                          ): (Int, Vector[Int]) = {
    // not happy using mutable objects
    val dinamicTriangle = triangle.map(_.to(ArrayBuffer)).to(ArrayBuffer)

    // Fase 1: down to up getting the sum
    for (row <- dinamicTriangle.length - 2 to 0 by -1) {
      for (column <- 0 until dinamicTriangle(row).length) {
        val currentPosition = dinamicTriangle(row)(column)
        val leftTree = dinamicTriangle(row + 1)(column)
        val rightTree = dinamicTriangle(row + 1)(column + 1)
        val minimalSum = currentPosition + math.min(leftTree, rightTree)
        dinamicTriangle(row)(column) = minimalSum
      }
    }

    // Fase 2: rebuilding the path
    var path = ArrayBuffer[Int]()
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

  def solveTriangleFromTop(
                                  triangulo: Vector[Vector[Int]]
                                ): (Int, Vector[Int]) = {
    // Unprotected, just testing the idea
    val cache = MutableMap.empty[(Int, Int), (Int, Vector[Int])]

    def resolver(row: Int, column: Int): (Int, Vector[Int]) = {
      cache.getOrElseUpdate(
        (row, column), {
          val current = triangulo(row)(column)

          if (row == triangulo.length - 1) {
            // base recursive case, the root of the tree
            (current, Vector(current))
          } else {
            // recursive calls for the tree
            val (suma1, camino1) = resolver(row + 1, column)
            val (suma2, camino2) = resolver(row + 1, column + 1)

            //choosing the lowest sum path
            if (suma1 < suma2) {
              (current + suma1, current +: camino1)
            } else {
              (current + suma2, current +: camino2)
            }
          }
        }
      )
    }

    // from top
    resolver(0, 0)
  }
}

package petroschallenge.solvers

import cats.effect.IO
import petroschallenge.model.MinimumTriangleSolver
import petroschallenge.model.Types.TriangleNode

import scala.collection.mutable.{Map => MutableMap}

object TopDownSolver extends MinimumTriangleSolver {
  def solveTriangle(
      triangulo: Vector[Vector[Int]]
  ): IO[TriangleNode] =
    IO.delay { // Unprotected, just testing the idea
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

package petroschallenge.solvers

import cats.effect.IO
import petroschallenge.model.Types.TriangleNode
import petroschallenge.model.{MinimumTriangleSolver}

object BottomUpSolver extends MinimumTriangleSolver {
  override def solveTriangle(triangle: Vector[Vector[Int]]): IO[TriangleNode] = IO.pure {
    require(triangle.nonEmpty && triangle.forall(_.nonEmpty), "Triangle must be non-empty")
    require(
      triangle.zipWithIndex.forall { case (row, i) => row.length == i + 1 },
      "Triangle must be well-formed"
    )

    val bottom: Vector[TriangleNode] =
      triangle.last.map(v => (v, Vector(v)))

    // Subimos fila a fila calculando el nuevo vector de (suma, camino)
    @annotation.tailrec
    def loop(rowIdx: Int, acc: Vector[TriangleNode]): TriangleNode = {
      if (rowIdx < 0) acc.head
      else {
        val row = triangle(rowIdx)
        val current: Vector[TriangleNode] =
          row.indices.map { j => combine(row(j), acc(j), acc(j + 1)) }.toVector

        loop(rowIdx - 1, current)
      }
    }

    loop(triangle.length - 2, bottom)
  }

  private def combine(current: Int, left: TriangleNode, right: TriangleNode): TriangleNode = {
    val (lsum, lpath) = left
    val (rsum, rpath) = right
    if (lsum <= rsum) (current + lsum, current +: lpath)
    else (current + rsum, current +: rpath)
  }

}

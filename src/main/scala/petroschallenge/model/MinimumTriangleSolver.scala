package petroschallenge.model

import cats.effect.IO
import petroschallenge.model.Types.TriangleNode

trait MinimumTriangleSolver {
  def solveTriangle(triangle: Vector[Vector[Int]]): IO[TriangleNode]
}

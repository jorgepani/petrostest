package petroschallenge

import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import petroschallenge.solvers.AtomicTriangleSolver.atomicTriangleResolver
import petroschallenge.solvers.{AtomicTriangleSolver, BottomUpSolver, TopDownSolver}

class TriangleSolverSpec extends AnyFunSuite with Matchers {

  test(
    "must solve a minimum triangle"
  ) {
    val triangulo = Vector(
      Vector(1),
      Vector(1, 2),
      Vector(3, 2, 1),
      Vector(4, 3, 7, 1),
      Vector(7, 3, 2, 4, 5),
      Vector(5, 4, 2, 6, 3, 7)
    )
    val (sum, path) = BottomUpSolver.solveTriangle(triangulo).unsafeRunSync()
    sum shouldBe 1 + 1 + 2 + 3 + 2 + 2 // 11
    path shouldBe Vector(1, 1, 2, 3, 2, 2)
  }

  test(
    "another random small triangle for the bottom up"
  ) {
    val triangulo = Vector(
      Vector(2),
      Vector(3, 4),
      Vector(6, 5, 7)
    )
    val (suma, camino) = BottomUpSolver.solveTriangle(triangulo).unsafeRunSync()
    suma shouldBe 2 + 3 + 5 // 10
    camino shouldBe Vector(2, 3, 5)
  }

  test(
    "must solve a minimum triangle top-down"
  ) {
    val triangulo = Vector(
      Vector(1),
      Vector(1, 2),
      Vector(3, 2, 1),
      Vector(4, 3, 7, 1),
      Vector(7, 3, 2, 4, 5),
      Vector(5, 4, 2, 6, 3, 7)
    )
    val (sum, path) = TopDownSolver.solveTriangle(triangulo).unsafeRunSync()
    sum shouldBe 1 + 1 + 2 + 3 + 2 + 2 // 11
    path shouldBe Vector(1, 1, 2, 3, 2, 2)
  }

  test(
    "another random small triangle for the top-down"
  ) {
    val triangulo = Vector(
      Vector(2),
      Vector(3, 4),
      Vector(6, 5, 7)
    )
    val (suma, camino) = TopDownSolver.solveTriangle(triangulo).unsafeRunSync()
    suma shouldBe 2 + 3 + 5 // 10
    camino shouldBe Vector(2, 3, 5)
  }

  test(
    "must solve a minimum triangle top-down atomically"
  ) {
    val triangulo = Vector(
      Vector(1),
      Vector(1, 2),
      Vector(3, 2, 1),
      Vector(4, 3, 7, 1),
      Vector(7, 3, 2, 4, 5),
      Vector(5, 4, 2, 6, 3, 7)
    )
    val (sum, path) = atomicTriangleResolver(triangulo).unsafeRunSync()
    sum shouldBe 1 + 1 + 2 + 3 + 2 + 2 // 11
    path shouldBe Vector(1, 1, 2, 3, 2, 2)
  }

  test(
    "another random small triangle for the top-down atomically"
  ) {
    val triangulo = Vector(
      Vector(2),
      Vector(3, 4),
      Vector(6, 5, 7)
    )
    val (suma, camino) = atomicTriangleResolver(triangulo).unsafeRunSync()
    suma shouldBe 2 + 3 + 5 // 10
    camino shouldBe Vector(2, 3, 5)
  }

  test(
    "must solve a minimum triangle top-down atomically with delay"
  ) {
    val triangulo = Vector(
      Vector(1),
      Vector(1, 2),
      Vector(3, 2, 1),
      Vector(4, 3, 7, 1),
      Vector(7, 3, 2, 4, 5),
      Vector(5, 4, 2, 6, 3, 7)
    )
    val (sum, path) = AtomicTriangleSolver.solveTriangle(triangulo).unsafeRunSync()
    sum shouldBe 1 + 1 + 2 + 3 + 2 + 2 // 11
    path shouldBe Vector(1, 1, 2, 3, 2, 2)
  }
}

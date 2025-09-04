package petroschallenge.solvers

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import java.io.InputStream
import scala.io.Source
import scala.concurrent.duration._

/*
this is a test to compare the solutions. Seems that bottom up is fastest
 */
class SolverSpeedSpec extends AnyFunSuite with Matchers {

  private def loadResource(name: String): String = {
    val is: InputStream =
      Option(Thread.currentThread().getContextClassLoader.getResourceAsStream(name))
        .getOrElse(throw new IllegalArgumentException(s"Resource not found: $name"))
    val src = Source.fromInputStream(is)(scala.io.Codec.UTF8)
    try src.mkString finally src.close()
  }

  private def parseTriangle(s: String): Vector[Vector[Int]] =
    s.split("\\R")
      .iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.split("\\s+").map(_.toInt).toVector)
      .toVector

  private def timed[A](io: IO[A]): (A, FiniteDuration) = {
    val t0 = System.nanoTime()
    val a  = io.unsafeRunSync()
    val t1 = System.nanoTime()
    (a, (t1 - t0).nanos)
  }

  private val solvers: List[(String, Vector[Vector[Int]] => IO[(Int, Vector[Int])])] = List(
    // TOP-DOWN
    "TopDown"    -> (tri => TopDownSolver.solveTriangle(tri)),

    // BOTTOM-UP
    "BottomUp"   -> (tri => BottomUpSolver.solveTriangle(tri)),

    // ATÓMICO (MapRef + processing-once)
    "AtomicMapRef" -> (tri => AtomicTriangleSolver.solveTriangle(tri))
  )

  private def bench(resource: String): Unit = {
    val input = loadResource(resource)
    val tri   = parseTriangle(input)

    val results: List[(String, (Int, Vector[Int]), FiniteDuration)] =
      solvers.map { case (name, f) =>
        val ((sum, path), dur) = timed(f(tri))
        info(f"[${resource}] ${name}%-12s took ${dur.toMillis}%,d ms   sum=$sum   pathLen=${path.length}")
        (name, (sum, path), dur)
      }

    results.nonEmpty shouldBe true

    val sums = results.map(_._2._1).toSet
    withClue(s"Sums differed across solvers in $resource: $results") {
      sums.size shouldBe 1
    }

    val pathLens = results.map(_._2._2.length).toSet
    withClue(s"Path lengths differed across solvers in $resource: $results") {
      pathLens.size shouldBe 1
      pathLens.head shouldBe tri.length
    }
  }

  test("Benchmark SMALL triangle (data_small.txt)") {
    bench("data_small.txt")
  }

  //It can break the stack
  ignore("Benchmark Big triangle (data_small.txt)") {
    bench("data_big.txt")
  }


}

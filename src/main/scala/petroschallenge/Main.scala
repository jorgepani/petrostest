package petroschallenge

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import petroschallenge.solvers.{AtomicTriangleSolver, BottomUpSolver}
import petroschallenge.util.Parser
import cats.syntax.all._
import fs2.io.stdin
import fs2.text

object Main extends IOApp.Simple {
  def run: IO[Unit] =
    readAllLines()
      .flatMap(parseTriangle)
      .flatMap(AtomicTriangleSolver.solveTriangle)
      .flatMap { case (sum, path) =>
        IO.println(s"""Minimal path is: ${path.mkString(" + ")} = $sum""")
      }
      .handleErrorWith(e => IO.println(s"Error: ${e.getMessage}"))

  def readAllLines(acc: List[String] = Nil): IO[List[String]] =
    Console[IO].readLine.attempt.flatMap {
      case Right(line)                   => readAllLines(acc :+ line)
      case Left(_: java.io.EOFException) => IO.pure(acc)
      case Left(e)                       => IO.raiseError(e)
    }

  def parseTriangle(lines: List[String]): IO[Vector[Vector[Int]]] =
    lines.toVector.traverse { line =>
      IO.fromEither(Parser.parseLine(line))
    }

  def runFs2: IO[Unit] =
    stdin[IO](bufSize = 64 * 1024)
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.trim)
      .filter(_.nonEmpty)
      .evalMap(line => IO.fromEither(Parser.parseLine(line)))
      .compile
      .toVector
      .flatMap(AtomicTriangleSolver.solveTriangle)
      .flatMap { case (sum, minimalPath) =>
        IO.println(s"Minimal path is: ${minimalPath.mkString(" + ")} = $sum")

      }
}

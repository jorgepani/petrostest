package petroschallenge

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import petroschallenge.solvers.BottomUpSolver
import petroschallenge.util.Parser
import cats.syntax.all._

object Main extends IOApp.Simple {
  def run: IO[Unit] =
    readAllLines()
      .flatMap(parseTriangle)
      .flatMap(BottomUpSolver.solveTriangle)
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
}

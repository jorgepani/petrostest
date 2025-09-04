package petroschallenge

import cats.effect.{IO, IOApp}
import fs2.io.stdin
import fs2.text
import petroschallenge.util.Parser

object Main extends IOApp.Simple {
  def run: IO[Unit] =
    stdin[IO](bufSize = 64 * 1024)
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.trim)
      .filter(_.nonEmpty)
      .evalMap(line => IO.fromEither(Parser.parseLine(line)))
      .compile
      .toVector
      .map(TriangleSolver.solveTriangleFromTop)
      .flatMap { case (suma, minimalPath) =>
        IO.println(s"Minimal path is: ${minimalPath.mkString(" + ")} = $suma")
      }
      .handleErrorWith(e => IO.println(s"Error: ${e.getMessage}"))
}

package petroschallenge

import cats.effect.{IO, IOApp}
import fs2.io.stdin
import fs2.text

object Main extends IOApp.Simple {
  def run: IO[Unit] =
    stdin[IO](bufSize = 64 * 1024)
      .through(text.utf8.decode)
      .through(text.lines)
      .fold(0)((acc, _) => acc + 1)
      .evalMap(n => IO.println(s"Total amount of línes: $n"))
      .compile.drain
}
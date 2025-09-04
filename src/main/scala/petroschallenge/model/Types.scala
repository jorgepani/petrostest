package petroschallenge.model

import cats.effect.{Deferred, IO}

object Types {
  type Key          = (Int, Int)
  type TriangleNode = (Int, Vector[Int])
  type Gate         = Deferred[IO, Either[Throwable, TriangleNode]]
}

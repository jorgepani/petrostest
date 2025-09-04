package petroschallenge.solvers

import cats.effect.kernel.Outcome
import cats.effect.std.MapRef
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all._
import petroschallenge.model.MinimumTriangleSolver
import petroschallenge.model.Types.{Gate, Key, TriangleNode}

object AtomicTriangleSolver extends MinimumTriangleSolver {

  def solveTriangle(triangle: Vector[Vector[Int]]): IO[TriangleNode] =
    MapRef.ofConcurrentHashMap[IO, Key, Gate]().flatMap { memo =>
      def go(row: Int, col: Int): IO[TriangleNode] = {
        val cell: Ref[IO, Option[Gate]] = memo((row, col)) // Ref per key

        // Safe node processing with IO.delay and yielding the control back to scheduler
        val compute: IO[TriangleNode] =
          IO.delay(triangle(row)(col)).flatMap { v =>
            if (row == triangle.length - 1) IO.pure((v, Vector(v)))
            else
              IO.cede *> (go(row + 1, col), go(row + 1, col + 1)).parMapN { (l, r) =>
                combine(v, l, r)
              }
          }

        for {
          existing <- cell.get
          out <- existing match {
            case Some(wait) => wait.get.rethrow
            case None =>
              for {
                gate <- Deferred[IO, Either[Throwable, TriangleNode]]
                claimed <- cell.modify {
                  case s @ Some(w) => (s, Left(w)) // other took the slot
                  case None        => (Some(gate), Right(gate))
                }
                res <- resolveClaim(claimed, compute, cell)
              } yield res
          }
        } yield out
      }

      go(0, 0)
    }

  def atomicTriangleResolver(
                              triangle: Vector[Vector[Int]]
                            ): IO[TriangleNode] = {
    Ref.of[IO, Map[Key, Deferred[IO, TriangleNode]]](Map.empty).flatMap { memo =>
      def go(row: Int, column: Int): IO[TriangleNode] = {
        val keyTuple = (row, column)

        //This is actually the logic over every single node
        val resolver: IO[TriangleNode] = {
          val v = triangle(row)(column)
          if (row == triangle.length - 1) IO.pure((v, Vector(v)))
          else
            (go(row + 1, column), go(row + 1, column + 1)).parMapN {
              case ((letfChildren, leftPath), (rightChildren, rightPath)) =>
                if (letfChildren <= rightChildren) (v + letfChildren, v +: leftPath)
                else (v + rightChildren, v +: rightPath)
            }
        }

        for {
          // is there any thread calculating?
          existing <- memo.get.map(mapa => mapa.get(keyTuple))
          out <- existing match {
            case Some(wait) => wait.get
            case None =>
              for {
                gate <- Deferred[IO, TriangleNode]
                claimed <- memo.modify { m =>
                  m.get(keyTuple) match {
                    case Some(w) => (m, Left(w))
                    case None => (m.updated(keyTuple, gate), Right(gate))
                  }
                }
                res <- claimed match {
                  case Left(w) => w.get
                  case Right(g) =>
                    resolver.attempt.flatTap {
                      case Right(v) => g.complete(v)
                      case Left(_) =>
                        memo.update(_ - keyTuple)
                    }.rethrow
                }
              } yield res
          }
        } yield out
      }

      go(0, 0) //Up to down
    }
  }


  /** Combines the root with the children to get the best sum */
  private def combine(currentValue: Int, left: TriangleNode, right: TriangleNode): TriangleNode =
    if (left._1 <= right._1) (currentValue + left._1, currentValue +: left._2)
    else (currentValue + right._1, currentValue +: right._2)

  /** Makes sure the computation is correct of releases the key otherwise */
  private def runAsWinner(
      compute: IO[TriangleNode],
      g: Gate,
      cell: Ref[IO, Option[Gate]]
  ): IO[TriangleNode] =
    IO.uncancelable { poll =>
      poll(compute).guaranteeCase {
        case Outcome.Succeeded(ioV) =>
          ioV.flatMap(v => g.complete(Right(v))).void
        case Outcome.Errored(e) =>
          g.complete(Left(e)) *> cell.set(None) // avoid blocking to other threads
        case Outcome.Canceled() =>
          g.complete(Left(new RuntimeException("Cancelled"))) *> cell.set(
            None
          ) // clean key on cancelation
      }
    }

  /** waits in case of loosing the race for the cell */
  private def resolveClaim(
      claimed: Either[Gate, Gate],
      compute: IO[TriangleNode],
      cell: Ref[IO, Option[Gate]]
  ): IO[TriangleNode] =
    claimed.fold(
      wait => wait.get.rethrow,
      g => runAsWinner(compute, g, cell)
    )

}

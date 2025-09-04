package petroschallenge

import cats.effect.kernel.Outcome
import cats.effect.std.MapRef
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => MutableMap}

object TriangleSolver {
  def solveFromBottomToTop(
      triangle: Vector[Vector[Int]]
  ): (Int, Vector[Int]) = {
    // not happy using mutable objects
    val dinamicTriangle = triangle.map(_.to(ArrayBuffer)).to(ArrayBuffer)

    // Fase 1: down to up getting the sum
    for (row <- dinamicTriangle.length - 2 to 0 by -1) {
      for (column <- 0 until dinamicTriangle(row).length) {
        val currentPosition = dinamicTriangle(row)(column)
        val leftTree        = dinamicTriangle(row + 1)(column)
        val rightTree       = dinamicTriangle(row + 1)(column + 1)
        val minimalSum      = currentPosition + math.min(leftTree, rightTree)
        dinamicTriangle(row)(column) = minimalSum
      }
    }

    // Fase 2: rebuilding the path
    var path          = ArrayBuffer[Int]()
    var currentColumn = 0

    // this is top down
    for (row <- triangle.indices) {
      path += triangle(row)(currentColumn)
      if (row < triangle.length - 1) {
        val valorInferior1 = dinamicTriangle(row + 1)(currentColumn)
        val valorInferior2 = dinamicTriangle(row + 1)(currentColumn + 1)
        if (valorInferior2 < valorInferior1) {
          currentColumn += 1
        }
      }
    }

    val total = dinamicTriangle(0)(0)
    (total, path.toVector)
  }

  def solveTriangleFromTop(
      triangulo: Vector[Vector[Int]]
  ): (Int, Vector[Int]) = {
    // Unprotected, just testing the idea
    val cache = MutableMap.empty[(Int, Int), (Int, Vector[Int])]

    def resolver(row: Int, column: Int): (Int, Vector[Int]) = {
      cache.getOrElseUpdate(
        (row, column), {
          val current = triangulo(row)(column)

          if (row == triangulo.length - 1) {
            // base recursive case, the root of the tree
            (current, Vector(current))
          } else {
            // recursive calls for the tree
            val (suma1, camino1) = resolver(row + 1, column)
            val (suma2, camino2) = resolver(row + 1, column + 1)

            //choosing the lowest sum path
            if (suma1 < suma2) {
              (current + suma1, current +: camino1)
            } else {
              (current + suma2, current +: camino2)
            }
          }
        }
      )
    }

    // from top
    resolver(0, 0)
  }

  type Key          = (Int, Int)
  type TriangleNode = (Int, Vector[Int])
  type Gate         = Deferred[IO, Either[Throwable, TriangleNode]]

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
            (go(row + 1, column), go(row + 1, column + 1)).parMapN { case ((s1, p1), (s2, p2)) =>
              if (s1 <= s2) (v + s1, v +: p1) else (v + s2, v +: p2)
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
                    case None    => (m.updated(keyTuple, gate), Right(gate))
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

  def atomicTriangleResolverWithDelay(triangle: Vector[Vector[Int]]): IO[TriangleNode] =
    MapRef.ofConcurrentHashMap[IO, Key, Gate]().flatMap { memo =>
      def goOncePerElement(row: Int, col: Int): IO[TriangleNode] = {
        val cell = memo((row, col))

        // This is actually the job done (IO.delay to wrap in a safe context)
        val compute: IO[TriangleNode] =
          IO.delay(triangle(row)(col)).flatMap { v =>
            if (row == triangle.length - 1) IO.pure((v, Vector(v)))
            else
              // Gives back the control to the scheduler to help
              IO.cede *> (goOncePerElement(row + 1, col), goOncePerElement(row + 1, col + 1))
                .parMapN { case ((s1, p1), (s2, p2)) =>
                  if (s1 <= s2) (v + s1, v +: p1) else (v + s2, v +: p2)
                }
          }

        for {
          existing <- cell.get
          out <- existing match {
            case Some(wait) =>
              // We wait and propagate with rethrow just in case
              wait.get.rethrow

            case None =>
              for {
                gate <- Deferred[IO, Either[Throwable, TriangleNode]]
                // Atomic booking
                claimed <- cell.modify {
                  case s @ Some(w) => (s, Left(w)) // we have to wait
                  case None        => (Some(gate), Right(gate))
                }
                res <- claimed match {
                  case Left(w)  => w.get.rethrow
                  case Right(g) =>
                    // I was worried about possible errors making other threads waiting forever
                    IO.uncancelable { poll =>
                      poll(compute).guaranteeCase {
                        case Outcome.Succeeded(ioV) =>
                          ioV.flatMap(v => g.complete(v.asRight)).void
                        case Outcome.Errored(e) =>
                          g.complete(Left(e)) *> cell.set(None)
                        case Outcome.Canceled() =>
                          g.complete(Left(new RuntimeException("Cancelled"))) *> cell.set(None)
                      }
                    }
                }
              } yield res
          }
        } yield out
      }

      goOncePerElement(0, 0)

    }

}

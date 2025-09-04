package petroschallenge.util

object Parser {
  def parseLine(line: String): Either[Throwable, Vector[Int]] =
    try Right(line.split("\\s+").filter(_.nonEmpty).map(_.toInt).toVector)
    catch {
      case _: NumberFormatException =>
        Left(new IllegalArgumentException(s"Not allowed number: '$line'"))
    }
}

package petroschallenge.util

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues

class ParserSpec extends AnyFunSuite with Matchers with EitherValues {

  test("parses simple spaces") {
    val Right(res) = Parser.parseLine("1 2 3")
    res shouldBe Vector(1, 2, 3)
  }

  test("parses more spaces than expected") {
    val Right(res) = Parser.parseLine("   10   20   30   ")
    res shouldBe Vector(10, 20, 30)
  }

  test("works with tabs") {
    val Right(res) = Parser.parseLine("4\t5\t6")
    res shouldBe Vector(4, 5, 6)
  }

  test("not so sure if allow negative numbers but...") {
    val Right(res) = Parser.parseLine("-4 -5 6")
    res shouldBe Vector(-4, -5, 6)
  }

  test("empty lines") {
    val Right(res) = Parser.parseLine("")
    res shouldBe Vector.empty[Int]
  }

  test("putting alpha chars -> Left(IllegalArgumentException)") {
    val err = Parser.parseLine("1 a 2").left.value
    err shouldBe a[IllegalArgumentException]
    err.getMessage shouldEqual "Not allowed number: '1 a 2'"
  }

  test("overflow -> Left(IllegalArgumentException)") {
    val err = Parser.parseLine("999999999999999999999").left.value
    err shouldBe a[IllegalArgumentException]
    err.getMessage should startWith("Not allowed number:")
  }
}
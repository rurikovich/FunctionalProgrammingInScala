package org.rurik.part1.part_1_4

import org.rurik.part1.part_1_4.Either.{sequence, traverse}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EitherSpec extends AnyFlatSpec with should.Matchers {
  val exception1 = new Exception("1")
  val exception2 = new Exception("2")

  "Either" should "operate correctly" in {
    val left: Either[Exception, Int] = Left(exception1)

    Right(1).map(_ + 2) shouldBe Right(3)
    left.map(_ + 2) shouldBe Left(exception1)

    val f: Int => Either[Exception, Int] = v => Right(v + 2)
    Right(1).flatMap(f) shouldBe Right(3)
    left.flatMap(f) shouldBe Left(exception1)


    Right(1).orElse(Right(2)) shouldBe Right(1)
    Left(exception1).orElse(Right(2)) shouldBe Right(2)

    val ff: (Int, Int) => Int = (v1, v2) => v1 + v2
    Right(1).map2(Right(2))(ff) shouldBe Right(3)
    Right(1).map2(Left(exception1))(ff) shouldBe Left(exception1)
    Left(exception1).map2(Left(exception2))(ff) shouldBe Left(exception1)
    Left(exception1).map2(Right(1))(ff) shouldBe Left(exception1)

  }


  "Either" should "operate traverse fn correctly" in {
    val fn: Int => Either[Exception, String] = a => if (a % 2 !== 0) Right(s"$a") else Left(exception1)

    traverse(List(1, 3, 5))(fn) shouldBe Right(List("1", "3", "5"))
    traverse(List(1, 2, 5))(fn) shouldBe Left(exception1)
  }

  "Either" should "operate sequence fn correctly" in {
    sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    sequence(List(Right(1), Left(exception1), Right(3))) shouldBe Left(exception1)
    sequence(List(Right(1), Left(exception2), Left(exception1), Right(3))) shouldBe Left(exception2)
  }

}
package org.rurik.part1.part1_5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StreamSpec extends AnyFlatSpec with should.Matchers {

  "Stream" should "operate toList correctly" in {

    Stream[Int](1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Stream" should "operate take correctly" in {
    Stream[Int]().take(10) shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).take(0) shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).take(3) shouldBe List(1, 2, 3)
    Stream[Int](1, 2, 3, 4, 5).take(5) shouldBe List(1, 2, 3, 4, 5)
    Stream[Int](1, 2, 3, 4, 5).take(10) shouldBe List(1, 2, 3, 4, 5)

  }

  "Stream" should "operate drop correctly" in {
    Stream[Int](1, 2, 3, 4, 5).drop(1).toList shouldBe List(2, 3, 4, 5)
    Stream[Int](1, 2, 3, 4, 5).drop(3).toList shouldBe List(4, 5)
    Stream[Int](1, 2, 3, 4, 5).drop(5).toList shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).drop(10).toList shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).drop(0).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Stream" should "operate takeWhile correctly" in {
    Stream[Int](1, 2, 3, 4, 5).takeWhile(_ <= 3).toList shouldBe List(1, 2, 3)
    Stream[Int](1, 2, 3, 4, 5).takeWhile(_ <= 0).toList shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).takeWhile(_ <= 10).toList shouldBe List(1, 2, 3, 4, 5)
    Stream[Int](1, 2, 3, 4, 5).takeWhile(_ <= -10).toList shouldBe List()


    Stream[Int](1, 2, 3, 4, 5).takeWhileFR(_ <= 3).toList shouldBe List(1, 2, 3)
    Stream[Int](1, 2, 3, 4, 5).takeWhileFR(_ <= 0).toList shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).takeWhileFR(_ <= 10).toList shouldBe List(1, 2, 3, 4, 5)
    Stream[Int](1, 2, 3, 4, 5).takeWhileFR(_ <= -10).toList shouldBe List()
  }

  "Stream" should "operate forAll correctly" in {
    Stream[Int](1, 2, 3, 4, 5).forAll(_ <= 3) shouldBe false
    Stream[Int](1, 2, 3, 4, 5).forAll(_ <= 6) shouldBe true
  }

  "Stream" should "operate headOption correctly" in {
    Stream[Int](1, 2, 3, 4, 5).headOption shouldBe Some(1)
    Stream[Int]().headOption shouldBe None
  }



}
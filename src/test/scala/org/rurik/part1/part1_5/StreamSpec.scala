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

    Stream[Int]().takeUnfold(10) shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).takeUnfold(0) shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).takeUnfold(3) shouldBe List(1, 2, 3)
    Stream[Int](1, 2, 3, 4, 5).takeUnfold(5) shouldBe List(1, 2, 3, 4, 5)
    Stream[Int](1, 2, 3, 4, 5).takeUnfold(10) shouldBe List(1, 2, 3, 4, 5)

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

    Stream[Int](1, 2, 3, 4, 5).takeWhileUnfold(_ <= 3).toList shouldBe List(1, 2, 3)
    Stream[Int](1, 2, 3, 4, 5).takeWhileUnfold(_ <= 0).toList shouldBe List()
    Stream[Int](1, 2, 3, 4, 5).takeWhileUnfold(_ <= 10).toList shouldBe List(1, 2, 3, 4, 5)
    Stream[Int](1, 2, 3, 4, 5).takeWhileUnfold(_ <= -10).toList shouldBe List()
  }

  "Stream" should "operate forAll correctly" in {
    Stream[Int](1, 2, 3, 4, 5).forAll(_ <= 3) shouldBe false
    Stream[Int](1, 2, 3, 4, 5).forAll(_ <= 6) shouldBe true
  }

  "Stream" should "operate headOption correctly" in {
    Stream[Int](1, 2, 3, 4, 5).headOption shouldBe Some(1)
    Stream[Int]().headOption shouldBe None
  }

  "Stream" should "operate map correctly" in {
    Stream[Int](1, 2, 3, 4, 5).map(a => a * 2).toList shouldBe List(2, 4, 6, 8, 10)
    Stream[Int]().map(a => a * 2).toList shouldBe List()

    Stream[Int](1, 2, 3, 4, 5).mapUnfold(a => a * 2).toList shouldBe List(2, 4, 6, 8, 10)
    Stream[Int]().mapUnfold(a => a * 2).toList shouldBe List()
  }

  "Stream" should "operate filter correctly" in {
    Stream[Int](1, 2, 3, 4, 5).filter(_ < 3).toList shouldBe List(1, 2)
    Stream[Int]().filter(_ < 3).toList shouldBe List()
  }

  "Stream" should "operate append correctly" in {
    Stream[Int](1, 2).append(Stream[Int](3, 4, 5)).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "Stream" should "operate flatMap correctly" in {
    Stream[Int](1, 2, 3).flatMap(a => Stream(a * 2)).toList shouldBe List(2, 4, 6)
    Stream[Int]().flatMap(a => Stream(a * 2)).toList shouldBe List()
  }

  "Stream" should "operate correctly with infinite stream fibs fn" in {
    Stream.fibs().take(10) shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    Stream.fibsUnfold().take(10) shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  "Stream" should "operate from correctly" in {
    Stream.from(3).take(5) shouldBe List(3, 4, 5, 6, 7)
    Stream.fromUnfold(3).take(5) shouldBe List(3, 4, 5, 6, 7)
  }

  "Stream" should "operate constant correctly" in {
    Stream.constant(3).take(5) shouldBe List(3, 3, 3, 3, 3)
    Stream.constantUnfold(3).take(5) shouldBe List(3, 3, 3, 3, 3)
  }

  "Stream" should "operate ones correctly" in {
    Stream.ones.take(5) shouldBe List(1, 1, 1, 1, 1)
    Stream.onesUnfold.take(5) shouldBe List(1, 1, 1, 1, 1)
  }

  "Stream" should "operate zipWith correctly" in {
    val s1 = Stream[Int](1, 2, 3)
    val s2 = Stream[Int](3, 2, 1, 1, 1)

    s1.zipWith(s2)(_ + _).toList shouldBe List(4, 4, 4)

    s1.zipAll(s2).toList shouldBe List(
      (Some(1), Some(3)),
      (Some(2), Some(2)),
      (Some(3), Some(1)),
      (None, Some(1)),
      (None, Some(1))
    )
  }

  "Stream" should "operate startsWith correctly" in {
    val s1 = Stream[Int](1, 2, 3, 4)
    val s2 = Stream[Int](1, 2)
    val s3 = Stream[Int](1, 2, 5)

    s1.startsWith(s2) shouldBe true
    s1.startsWith(s3) shouldBe false
  }

  "Stream" should "operate tails correctly" in {
    val s1 = Stream[Int](1, 2, 3, 4)

    s1.tails.map(_.toList).toList shouldBe List(
      List(1, 2, 3, 4),
      List(2, 3, 4),
      List(3, 4),
      List(4),
      List()
    )

  }


}

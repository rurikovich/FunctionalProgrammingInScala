package org.rurik.part1.part1_4

import org.rurik.part1.part1_4.Option.{map2, sequence, traverse, variance}
import org.scalatest.flatspec._
import org.scalatest.matchers._

import scala.util.Try

class Exercise4_1Spec extends AnyFlatSpec with should.Matchers {

  "Option" should "operate correctly" in {
    val none: Option[Int] = None

    Some(1).map(_ + 1) shouldBe Some(2)
    none.map(_ + 1) shouldBe None

    Some(1).flatMap(a => Some(a + 1)) shouldBe Some(2)
    none.flatMap(a => Some(a + 1)) shouldBe None

    Some(1).getOrElse(0) shouldBe 1
    none.getOrElse(0) shouldBe 0

    Some(1).orElse(Some(0)) shouldBe Some(1)
    none.orElse(Some(0)) shouldBe Some(0)

    Some(1).filter(_ > 0) shouldBe Some(1)
    none.filter(_ > 0) shouldBe None

  }

  "variance" should "operates correctly" in {
    variance(List(2, 4, 4, 4, 5, 5, 7, 9)) shouldBe Some(4.0)
  }


  "map2" should "operates correctly" in {
    map2(None: Option[Int], None: Option[Int])(_ + _) shouldBe None
    map2(Some(2), None: Option[Int])(_ + _) shouldBe None
    map2(None: Option[Int], Some(2))(_ + _) shouldBe None
    map2(Some(2), Some(2))(_ + _) shouldBe Some(4)
  }

  "sequence" should "operates correctly" in {
    sequence(List(None)) shouldBe None
    sequence(List(Some(1), Some(1), Some(2))) shouldBe Some(List(1, 1, 2))
    sequence(List(Some(1), Some(1), Some(2), None)) shouldBe None
  }

  "traverse" should "operates correctly" in {
    val f: Int => Option[String] = i => if (i % 2 !== 0) Some(s"$i") else None

    traverse(List(2))(f) shouldBe None
    traverse(List(1, 3, 4))(f) shouldBe None
    traverse(List(1, 3, 5, 7))(f) shouldBe Some(List("1", "3", "5", "7"))

  }

}

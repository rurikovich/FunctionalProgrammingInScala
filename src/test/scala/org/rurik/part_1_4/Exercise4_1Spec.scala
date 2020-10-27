package org.rurik.part_1_4

import org.scalatest.flatspec._
import org.scalatest.matchers._

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

}

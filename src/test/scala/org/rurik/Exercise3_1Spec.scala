package org.rurik

import org.rurik.Exercise3_1.List._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import org.rurik.Exercise3_1._

class Exercise3_1Spec extends AnyFlatSpec with should.Matchers {


  "List" should "drop correctly" in {
    drop(List(1, 2, 3, 4, 5), 3) shouldBe List(4, 5)
    drop(List(), 3) shouldBe List()
    drop(List(1), 3) shouldBe List()
    drop(Nil, 3) shouldBe Nil
  }

  "List" should "init correctly" in {
    init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
    init(List(1, 2)) shouldBe List(1)
    init(List(1)) shouldBe List()
    init(List()) shouldBe List()
    init(Nil) shouldBe Nil
  }

  "List" should "calculate length correctly" in {
    List.length(List(1, 2, 3, 4, 5)) shouldBe 5
    List.length(List(1)) shouldBe 1
    List.length(List()) shouldBe 0

    List.lengthFL(List(1, 2, 3, 4, 5)) shouldBe 5
    List.lengthFL(List(1)) shouldBe 1
    List.lengthFL(List()) shouldBe 0

  }

}

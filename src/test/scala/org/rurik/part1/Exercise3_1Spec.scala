package org.rurik.part1

import org.rurik.List._
import org.rurik.{List, Nil}
import org.scalatest.flatspec._
import org.scalatest.matchers._

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

    lengthFL(List(1, 2, 3, 4, 5)) shouldBe 5
    lengthFL(List(1)) shouldBe 1
    lengthFL(List()) shouldBe 0

  }

  "List" should "reverse correctly" in {
    reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
    reverse(List(1, 2, 3, 4, 5, 6, 7)) shouldBe List(7, 6, 5, 4, 3, 2, 1)
    reverse(List(1, 2)) shouldBe List(2, 1)
    reverse(List(1)) shouldBe List(1)
    reverse(List()) shouldBe List()
  }

  "List" should "append correctly" in {
    append(List(1, 2), List(3, 4, 5)) shouldBe List(1, 2, 3, 4, 5)
  }

  "List" should "concat correctly" in {
    concat(
      List(List(1, 2), List(3, 4, 5))
    ) shouldBe List(1, 2, 3, 4, 5)

    concat(
      List(List(1, 2), List(3, 4, 5), List(6, 7))
    ) shouldBe List(1, 2, 3, 4, 5, 6, 7)

    concat(List()) shouldBe List()

    concat(List(List(1, 2), List())) shouldBe List(1, 2)

  }

  "List" should "add1 correctly" in {
    add1(List(1, 2, 3, 4, 5, 6, 7)) shouldBe List(2, 3, 4, 5, 6, 7, 8)
  }

  "List" should "doubleToString correctly" in {
    doubleToString(List(1, 2, 3, 4, 5)) shouldBe List("1.0", "2.0", "3.0", "4.0", "5.0")
  }

  "List" should "filter odd numbers correctly" in {
    filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
    filterFM(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
  }

  "List" should "flatMap  correctly" in {
    flatMap(List(1, 2))(i => List(i, i)) shouldBe List(1, 1, 2, 2)

    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "List" should "sumLists  correctly" in {
    sumLists(List(1, 2), List(3, 4)) shouldBe List(4, 6)

    zipWith(List(1, 2), List(3, 4))(_ + _) shouldBe List(4, 6)
  }

  "List" should "hasSubsequence  correctly" in {
    hasSubsequence(List(1, 2, 3, 4, 5), List()) shouldBe true
    hasSubsequence(List(1, 2, 3, 4, 5), List(1)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4, 5), List(3, 4)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3, 4)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5)) shouldBe true
    hasSubsequence(List(1, 2, 3, 7, 3, 4, 5), List(3, 4, 5)) shouldBe true

    hasSubsequence(List(1, 2, 3, 7, 4, 5), List(8, 9)) shouldBe false
    hasSubsequence(List(1, 2, 3, 7, 4, 5), List(3, 4, 5)) shouldBe false
    hasSubsequence(List(1, 2, 3, 7, 4, 5), List(3, 4)) shouldBe false
    hasSubsequence(List(1, 2, 3), List(1, 2, 3, 4, 5)) shouldBe false
    hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4, 5, 6)) shouldBe false

  }

}

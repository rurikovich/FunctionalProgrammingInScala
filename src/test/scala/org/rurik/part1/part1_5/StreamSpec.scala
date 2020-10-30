package org.rurik.part1.part1_5

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StreamSpec extends AnyFlatSpec with should.Matchers {

  "Stream" should "operate toList correctly" in {

    Stream[Int](1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)

  }


}
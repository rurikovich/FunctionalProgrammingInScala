package org.rurik.part3.chapter11

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class MonadSpec extends AnyFlatSpec with Checkers with should.Matchers {

  "State monad" should "zipWithIndex correctly" in {
    val list = List("a", "b", "c")
    Monad.zipWithIndex(list) shouldEqual List((0, "a"), (1, "b"), (2, "c"))
  }

}

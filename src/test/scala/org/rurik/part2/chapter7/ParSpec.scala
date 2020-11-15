package org.rurik.part2.chapter7

import java.util.concurrent.Executors

import org.rurik.part2.chapter7.Par.Par
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ParSpec extends AnyFlatSpec with should.Matchers {

  val es = Executors.newFixedThreadPool(2)

  "Combinators" should "operate correctly" in {

    val n = 1000

    val i: Int = rndInt(n)
    val parInt = Par.lazyUnit(i)

    val parList: List[Par[Int]] = (0 to n).map(Par.lazyUnit(_)).toList

    Par.run(es)(Par.choiceN(parInt)(parList)).get() shouldBe i

  }

  def rndInt(end: Int): Int = {
    val rnd = new scala.util.Random
    0 + rnd.nextInt((end - 0) + 1)
  }


}

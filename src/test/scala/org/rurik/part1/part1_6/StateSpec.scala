package org.rurik.part1.part1_6

import org.rurik.part1.part1_6.State._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StateSpec extends AnyFlatSpec with should.Matchers {

  "State" should "operate correctly with all functions" in {
    val rnd: RNG = SimpleRNG(123)
    val randA: Rand[Int] = unit(1)
    val randB: Int => Rand[String] = a => unit(s"$a")

    unit[RNG, Int](1).map(_ * 2).run(rnd)._1 shouldBe 2
    map2[RNG, Int, Int, Int](unit(1), unit(2))(_ + _).run(rnd)._1 shouldBe 3
    randA.flatMap[String](randB).run(rnd)._1 shouldBe "1"
  }

  "State" should "operate correctly with sequence" in {
    val rnd1: RNG = SimpleRNG(1)
    val rnd2: RNG = SimpleRNG(2)
    val rnd3: RNG = SimpleRNG(3)
    val rndList: List[(Int, RNG)] = List(rnd1, rnd2, rnd3).map(_.nextInt)

    val randList: List[Rand[Int]] = rndList.map((rnd: (Int, RNG)) => State((_: RNG) => rnd))
    sequence[RNG, Int](randList).run(SimpleRNG(0))._1 shouldBe rndList.map(_._1)
  }

}

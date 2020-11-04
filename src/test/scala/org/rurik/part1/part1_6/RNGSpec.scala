package org.rurik.part1.part1_6

import org.rurik.part1.part1_6.RNG._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RNGSpec extends AnyFlatSpec with should.Matchers {

  "nonNegativeInt" should "return only positive numbers" in {
    nonNegativeInt(SimpleRNG(1))._1 > 0 shouldBe true
  }


  "sequence" should "operate correctly" in {
    val rnd1 = SimpleRNG(1)
    val rnd2 = SimpleRNG(2)
    val rnd3 = SimpleRNG(3)
    val rndList: List[(Int, RNG)] = List(rnd1, rnd2, rnd3).map(_.nextInt)

    val randList: List[Rand[Int]] = rndList.map((rnd: (Int, RNG)) => (_: RNG) => rnd)
    sequence(randList)(SimpleRNG(0))._1 shouldBe rndList.map(_._1)
  }

  "ints" should "operate correctly" in {
    val simpleRNG = SimpleRNG(0)

    val (d1, r1) = simpleRNG.nextInt
    val (d2, r2) = r1.nextInt

    val (d3, _) = r2.nextInt
    ints(3)(simpleRNG)._1 shouldBe List(d1, d2, d3)
    intsSequence(3)(simpleRNG)._1 shouldBe List(d1, d2, d3)
  }


  "flatMap" should "operate correctly" in {
    val simpleRNG = SimpleRNG(0)
    val randA: Rand[Int] = unit(1)
    val randB: Int => Rand[String] = a => unit(s"$a")

    flatMap[Int, String](randA)(randB)(simpleRNG)._1 shouldBe "1"
  }


  "nonNegativeLessThan" should "operate correctly" in {
    val simpleRNG = SimpleRNG(123)
    val n = 3

    val rand = nonNegativeLessThan(n)

    val (v1, r1) = rand(simpleRNG)
    val (v2, r2) = rand(r1)
    val (v3, r3) = rand(r2)
    val (v4, _) = rand(r3)

    (v1 >= 0 && v1 < n) shouldBe true
    (v2 >= 0 && v2 < n) shouldBe true
    (v3 >= 0 && v3 < n) shouldBe true
    (v4 >= 0 && v4 < n) shouldBe true
  }

  "map" should "operate correctly" in {
    val rnd = SimpleRNG(123)

    map(unit(1))(_ * 2)(rnd)._1 shouldBe 2
    mapFM(unit(1))(_ * 2)(rnd)._1 shouldBe 2
  }


  "map2" should "operate correctly" in {
    val rnd = SimpleRNG(123)

    map2(unit(1), unit(2))(_ + _)(rnd)._1 shouldBe 3
    map2FM(unit(1), unit(2))(_ + _)(rnd)._1 shouldBe 3
  }

}

package org.rurik.part2.chapter8

import org.rurik.part1.part1_6.{RNG, SimpleRNG, State}
import org.rurik.part2.chapter8.Gen.{choose, intGen}
import org.scalacheck
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.Checkers

class GenSpec extends AnyFlatSpec with Checkers with should.Matchers {

  val rng = SimpleRNG(1)

  "choose" should "gen new values between start and end correctly" in {
    val startGen = org.scalacheck.Gen.choose(0, 100)
    val endGen = org.scalacheck.Gen.choose(101, 200)

    check {
      forAll(startGen, endGen) {
        case (start, end) =>
          val v = choose(start, end).sample.run(rng)._1
          v > start && v < end
      }
    }
  }


  val intGen: Gen[Int] = Gen(State(_.nextInt))

  "listOfN" should "gen list of size n correctly" in {
    val nStart = 5
    val nEnd = 100
    val nGen = org.scalacheck.Gen.choose(nStart, nEnd)

    check {
      forAll(nGen) {
        n: Int =>
          val listGen: Gen[List[Int]] = Gen.listOfN(n, intGen)
          val list: List[Int] = listGen.sample.run(rng)._1
          list.length == n
      }
    }


    check {
      forAll(nGen) {
        n: Int =>
          val listGen: Gen[List[Int]] = intGen.listOfN(n)
          val list: List[Int] = listGen.sample.run(rng)._1
          list.length == n
      }
    }
  }


  "flatMap" should "operate correctly" in {

    def genStr(v: Int): Gen[String] =
      Gen[String] {
        State {
          rnd =>
            val (i, newRnd) = intGen.sample.run(rnd)
            (s"($i,$v)", newRnd)
        }
      }


    val generatedStr = intGen.flatMap(genStr).sample.run(rng)._1
    val generatedInt = intGen.sample.run(rng)._1

    generatedStr should endWith(s",$generatedInt)")
  }


  val g1: Gen[Int] = Gen(State(_.nextInt))
  val g2: Gen[Int] = Gen(State {
    _.nextInt match {
      case (v, rng) => (v + (v % 3), rng)
    }
  })

  "union" should "operate correctly" in {


    val listOfG1orG2generatedValuesWithRNG: Seq[(Int, SimpleRNG)] = (1 until 2_000_000).map(SimpleRNG(_)).map {
      rng => (Gen.union(g1, g2).sample.run(rng)._1, rng)
    }

    val g1Likelihood = likelihood(listOfG1orG2generatedValuesWithRNG, g1)
    val g2Likelihood = likelihood(listOfG1orG2generatedValuesWithRNG, g2)

    val eps = 0.001
    Math.abs(g1Likelihood - g2Likelihood) should be < eps

  }

  "weighted" should "operate correctly" in {
    val w1Gen = org.scalacheck.Gen.choose(1, 1000)
    val w2Gen = org.scalacheck.Gen.choose(1, 1000)

    val eps = 0.001
    val samplesCount = 2_000_000

    check {
      forAll(w1Gen, w2Gen) {
        case (w1, w2) =>
          val (g1Likelihood, g2Likelihood) = computeWeightedGenLikelihoods(g1, w1, g2, w2, samplesCount)
          Math.abs(g1Likelihood - g2Likelihood) < eps
      }
    }

  }


  def computeWeightedGenLikelihoods(g1: Gen[Int], w1: Double, g2: Gen[Int], w2: Double, samplesCount: Int): (Likelihood, Likelihood) = {
    def listOfG1orG2generatedValuesWithRNG(samplesCount: Int): Seq[(Int, SimpleRNG)] = (0 until samplesCount).map(SimpleRNG(_)).map {
      rng => (Gen.weighted((g1, w1), (g2, w2)).sample.run(rng)._1, rng)
    }

    val g1Likelihood: Likelihood = likelihood(listOfG1orG2generatedValuesWithRNG(samplesCount), g1)
    val g2Likelihood: Likelihood = likelihood(listOfG1orG2generatedValuesWithRNG(samplesCount), g2)

    (g1Likelihood, g2Likelihood)
  }


  type Likelihood = Double

  def likelihood(generatedValuesList: Seq[(Int, SimpleRNG)], gen: Gen[Int]): Likelihood = {
    val count = generatedValuesList.count {
      case (v, rng) =>
        val genV = gen.sample.run(rng)._1
        v == genV
    }.toDouble
    count / generatedValuesList.size
  }


}

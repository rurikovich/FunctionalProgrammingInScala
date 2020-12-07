package org.rurik.part2.chapter8

import org.rurik.part1.part1_6.RNG.Rand
import org.rurik.part1.part1_6.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOf1(n: Int): Gen[List[A]] = listOfN(n max 1)

  def listOfN(size: Int): Gen[List[A]] = flatMap {
    a: A =>
      Gen[List[A]] {
        State {
          rnd =>
            val list = (0 until size).map {
              _ => sample.run(rnd)._1
            }.toList
            (list, rnd)
        }
      }
  }

  def unsized: SGen[A] = SGen[A](
    _ => this
  )

}

case class SGen[A](forSize: Int => Gen[A])

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen[List[A]](i => g.listOfN(i))
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen[Int](State(RNG.nonNegativeBetween(start, stopExclusive)(_)))
  }

  def unit[A](a: => A): Gen[A] = Gen[A](State(RNG.unit(a)(_)))

  /*
  проблема в том что мне хочется чтобы g1 и g2 выполнялись на том же RNG что и был передан в функцию изначально.
  Это нужно для тестирования
  возможно это можно решить тестируя на  newRng  из `val (i, newRng) = rng.nextInt`
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  /*
  проблема в том что мне хочется чтобы g1 и g2 выполнялись на том же RNG что и был передан в функцию изначально.
  Это нужно для тестирования
  возможно это можно решить тестируя на  newRng  из `val (i, newRng) = rng.nextInt`
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = intGen.flatMap {
    i =>
      val (gen1, w1) = g1
      val (gen2, w2) = g2

      val step: Double = intervalStep(w1, w2)
      val endOfW1Interval = step * w1

      if (absInt(i) <= endOfW1Interval) gen1 else gen2
  }

  /*
   делим всю длину int'а на кол-во частей равное сумме весов.
   т.е. если w1=3 а w2=6, то делим интервал возможных значений Int на 9 частей
    */
  private def intervalStep(w1: Double, w2: Double): Double = {
    val sumW = w1 + w2
    val start = Int.MinValue
    val end = Int.MaxValue

    val fullLengt = -start + end
    fullLengt / sumW
  }

  /*
  вычисляем положительное значение для i, если принять Int.MinValue за начало отсчета
   */
  private def absInt[A](i: Int) = {
    if (i < 0) -i else -Int.MinValue + i
  }

  def intGen: Gen[Int] = {
    Gen[Int](State(_.nextInt))
  }

  def boolean: Gen[Boolean] = Gen[Boolean](State(RNG.map(RNG.int)(_ > 0)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen[List[A]](
    State(
      (rnd: RNG) => {
        val randList: List[Rand[A]] = (0 until n).map {
          _ => (r: RNG) => g.sample.run(r)
        }.toList

        RNG.sequence(randList)(rnd)
      }
    )
  )
}

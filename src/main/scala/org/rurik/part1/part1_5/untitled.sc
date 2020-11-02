import org.rurik.part1.part1_5.Stream

val ones: Stream[Int] = Stream.cons(1, ones)

ones.take(5).toList

ones.takeWhile(_ == 1)

ones.forAll(_ != 1)

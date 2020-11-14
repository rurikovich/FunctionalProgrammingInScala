package org.rurik.part2.chapter7

import java.util.concurrent.Executors

import org.rurik.part2.chapter7.ParNonBlocking._

object ParRunner extends App {

  val p = parMap(List.range(1, 4_000)) {
    i =>
      (i,math.sqrt(i))
        }

  val x = run(Executors.newFixedThreadPool(1))(p)

  println(x)

}

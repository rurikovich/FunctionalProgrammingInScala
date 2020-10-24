package org.rurik

object Exercise2_2 {

  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n > arr.length - 2) {
        true
      } else if (ordered(arr(n), arr(n + 1))) {
        loop(n + 1)
      } else {
        false
      }
    }

    loop(0)
  }

}

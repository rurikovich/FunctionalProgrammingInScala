package org.rurik.part1.part1_6

import org.rurik.part1.part1_6.State.unit

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a: A, s1: S) = run(s)
      f(a).run(s1)
    }
  )

  def map[B](f: A => B): State[S, B] = flatMap[B](a => unit(f(a)))

}

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

  def map2[S, A, B, C](sA: State[S, A], sB: State[S, B])(f: (A, B) => C): State[S, C] =
    sA.flatMap[C] {
      a: A =>
        State[S, C]((s: S) => {
          val (b, s1) = sB.run(s)
          (f(a, b), s1)
        })

    }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldLeft(State((List.empty[A], _: S))) {
    (list: State[S, List[A]], s: State[S, A]) =>
      map2(list, s)(_ ::: List(_))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

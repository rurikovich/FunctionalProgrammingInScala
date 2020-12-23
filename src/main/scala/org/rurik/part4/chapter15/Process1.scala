package org.rurik.part4.chapter15

import java.io.File

import cats.effect.IO
import org.rurik.part4.chapter15.Process1.{count, exists, lift}

import scala.collection.immutable.LazyList.#::

sealed trait Process1[I, O] {

  def apply(s: LazyList[I]): LazyList[O] = this match {
    case Halt() => LazyList()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process1[I, O] = {
    def go(p: Process1[I, O]): Process1[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def |>[O2](p2: Process1[O, O2]): Process1[I, O2] = {
    p2 match {
      case Halt() => Halt()
      case Emit(h: O2, t: Process1[I, O]) => Emit(h, this |> t)
      case Await(f) => this match {
        case Emit(h, t) => t |> f(Some(h))
        case Halt() => Halt[I, O]() |> f(None)
        case Await(g) => Await((i: Option[I]) => g(i) |> p2)
      }
    }
  }

  def map[O2](f: O => O2): Process1[I, O2] = this |> lift(f)

  def ++(p: => Process1[I, O]): Process1[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process1[I, O2]): Process1[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }


}


case class Emit[I, O](head: O, tail: Process1[I, O] = Halt[I, O]()) extends Process1[I, O]

case class Await[I, O](recv: Option[I] => Process1[I, O]) extends Process1[I, O]

case class Halt[I, O]() extends Process1[I, O]


object Process1 {

  def any: Process1[Boolean, Boolean] =
    loop(false)((b: Boolean, s) => (s || b, s || b))

  def exists[I](f: I => Boolean): Process1[I, Boolean] =
    lift(f) |> any

  def liftOne[I, O](f: I => O): Process1[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process1[I, O] = liftOne(f).repeat

  def await[I, O](f: I => Process1[I, O],
                  fallback: Process1[I, O] = Halt[I, O]()): Process1[I, O] =
    Await[I, O] {
      case Some(i) => f(i)
      case None => fallback
    }

  def filter[I](p: I => Boolean): Process1[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _ => Halt()
    }.repeat


  def take[I](n: Int): Process1[I, I] = n match {
    case 0 => Halt()
    case n => await(i => Emit[I, I](i, take(n - 1)))
  }

  /* The identity `Process`, just repeatedly echos its input. */
  def id[I]: Process1[I, I] = lift(identity)

  def drop[I](n: Int): Process1[I, I] = n match {
    case i if i <= 0 => id
    case _ => Await(_ => drop(n - 1))

  }

  def takeWhile[I](f: I => Boolean): Process1[I, I] = await {
    i =>
      if (f(i)) Emit(i, takeWhile(f)) else Halt()
  }

  def dropWhile[I](f: I => Boolean): Process1[I, I] = await {
    i =>
      if (f(i)) dropWhile(f) else Emit(i, id)
  }

  def count[I]: Process1[I, Int] = {
    def go(n: Int): Process1[I, Int] = {
      await {
        _ => Emit(n + 1, go(n + 1))
      }
    }

    go(0)
  }


  def mean: Process1[Double, Double] = {
    def go(n: Int, sum: Double): Process1[Double, Double] = {
      await {
        i => Emit(sum / n + 1, go(n + 1, sum + i))
      }
    }

    go(0, 0.0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process1[I, O] =
    await(
      (i: I) =>
        f(i, z) match {
          case (o, s2) => Emit(o, loop(s2)(f))
        }
    )

  def processFile[A, B](f: java.io.File,
                        p: Process1[String, A],
                        z: B)(g: (B, A) => B): IO[B] = IO {
    @annotation.tailrec
    def go(ss: Iterator[String], cur: Process1[String, A], acc: B): B =
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext)
            recv(Some(ss.next))
          else
            recv(None)
          go(ss, next, acc)

        case Emit(h, t) =>
          go(ss, t, g(acc, h))
      }

    val s = io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }


}

object FileReaderApp extends App {

  val f = new File("src/main/resources/big_file.txt")

  private val process: Process1[String, Boolean] = count[String] |> exists(_ > 26)

  val io = Process1.processFile[Boolean, Boolean](f, process, false)(_ || _)

  val res = io.unsafeRunSync()

  println(res)

}
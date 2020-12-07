package org.rurik.part3.chapter11

case class Id[A](value: A) {

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  def map[B](f: A => B): Id[B] = Id[B](f(value))

}

object Id {

  val IdMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id[A](a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }
}

package org.rurik.part2.chapter9.v1

case class JsonParser[+A](run: String => Either[Throwable, A])

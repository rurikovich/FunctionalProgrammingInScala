package org.rurik.part2.chapter9.v2.parsers

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] = latest map (_._1)

  def latest: Option[(Location, String)] = stack.lastOption

}

package org.rurik.part2.chapter9.v2

import scala.util.matching.Regex

object RegexPatterns {

  val numberPattern: Regex = "^[0-9]+(\\.[0-9]+)?".r

  val stringPattern: Regex = "^\\\"[a-z,A-Z]*\\\"".r


}

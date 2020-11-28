package org.rurik.part2.chapter9

import org.rurik.part2.chapter9.ParserExamples.{ExampleParseError, ExampleParser}
import org.rurik.part2.chapter9.v1.Parsers

trait ParserExamples extends Parsers[ExampleParseError, ExampleParser] {

  char('a').many().slice().map(_.size) ** char('b').many1().slice().map(_.size)

}

object ParserExamples {
  type ExampleParseError = Exception

  class ExampleParser[+A]()

}

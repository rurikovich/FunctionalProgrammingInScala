package org.rurik.part2.chapter9.helpers

case class BoolHelper(b: Boolean) {

  def toOpt: Option[Boolean] = if (b) Some(b) else None

}

object BoolHelper {
  implicit def boolToBoolHelper(b: Boolean): BoolHelper = BoolHelper(b)
}

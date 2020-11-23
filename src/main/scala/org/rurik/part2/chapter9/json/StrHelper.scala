package org.rurik.part2.chapter9.json

import org.rurik.part2.chapter9.json.BoolHelper.boolToOpt
import org.rurik.part2.chapter9.json.StrHelper.strToStrHelper


object BoolHelper {
  def boolToOpt(b: Boolean): Option[Boolean] = if (b) Some(b) else None
}

case class StrHelper(str: String) {

  def withoutQuotes(): String = str.substring(1, str.length - 1)

  def withoutFrameAndDelimeters(leftFrame: String, rightFrame: String, delimeter: String): Option[List[String]] = boolToOpt(str.framedBy(leftFrame, rightFrame)).map {
    _ =>
      val lFrameLength = leftFrame.length
      val rFrameLength = rightFrame.length
      val strWithoutFrame = str.substring(lFrameLength, str.length - rFrameLength)
      strWithoutFrame.split(delimeter).toList
  }

  def withoutFrame(frame: String): Option[String] = boolToOpt(str.framedBy(frame)).map {
    _ =>
      val frameLength = frame.length
      str.substring(frameLength, str.length - frameLength)
  }

  def framedBy(frame: String): Boolean = {
    str.startsWith(frame) && str.endsWith(frame)
  }

  def framedBy(leftFrame: String, rightFrame: String): Boolean = {
    str.startsWith(leftFrame) && str.endsWith(rightFrame)
  }

}

object StrHelper {
  implicit def strToStrHelper(str: String): StrHelper = StrHelper(str)
}

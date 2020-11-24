package org.rurik.part2.chapter9.json

import org.rurik.part2.chapter9.json.StrHelper.{quote, strToStrHelper}
import BoolHelper._

case class StrHelper(str: String) {

  def withoutQuotes(): String = str.strip().substring(1, str.length - 1)

  def withoutFrameAndDelimeters(leftFrame: String, rightFrame: String, delimeter: String): Option[List[String]] = {
    val s = str.strip()
    s.framedBy(leftFrame, rightFrame).toOpt().map {
      _ =>
        val lFrameLength = leftFrame.length
        val rFrameLength = rightFrame.length
        val strWithoutFrame = s.substring(lFrameLength, s.length - rFrameLength)
        strWithoutFrame.split(delimeter).map(_.strip).toList
    }

  }

  def withoutFrame(frame: String): Option[String] = str.framedBy(frame).toOpt().map {
    _ =>
      val frameLength = frame.length
      str.strip().substring(frameLength, str.length - frameLength)
  }

  /*
    после вычитания рамок из строки должен оставаться хотя бы 1 символ
   */
  def framedBy(leftFrame: String, rightFrame: String): Boolean = {
    val oneChar = 1
    str.strip().startsWith(leftFrame) && str.endsWith(rightFrame) && (leftFrame.length + rightFrame.length + oneChar <= str.length)
  }

  def framedBy(frame: String): Boolean = framedBy(frame, frame)

  def quoted: String = s"""$quote$str$quote""".stripMargin

}

object StrHelper {

  val quote = "\""

  implicit def strToStrHelper(str: String): StrHelper = StrHelper(str)
}

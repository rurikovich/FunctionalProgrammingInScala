package org.rurik.part2.chapter9.helpers

import org.rurik.part2.chapter9.helpers.StrHelper._

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
        strWithoutFrame.strip().split(delimeter).map(_.strip).toList.filter(_.nonEmpty)
    }

  }

  def withoutFrame(frame: String): Option[String] = str.nonEmptyFramedBy(frame).toOpt.map {
    _ =>
      val frameLength = frame.length
      str.strip().substring(frameLength, str.length - frameLength)
  }

  /*
    после вычитания рамок из строки должен оставаться хотя бы 1 символ
   */
  def nonEmptyFramedBy(leftFrame: String, rightFrame: String): Boolean = {
    val oneChar = 1
    str.strip().startsWith(leftFrame) && str.endsWith(rightFrame) && (leftFrame.length + rightFrame.length + oneChar <= str.length)
  }

  def nonEmptyFramedBy(frame: String): Boolean = nonEmptyFramedBy(frame, frame)


  def framedBy(leftFrame: String, rightFrame: String): Boolean = {
    str.strip().startsWith(leftFrame) && str.endsWith(rightFrame) && (leftFrame.length + rightFrame.length <= str.length)
  }

  def framedBy(frame: String): Boolean = nonEmptyFramedBy(frame, frame)


  def quoted: String = s"""$quote$str$quote""".stripMargin


}

object StrHelper {

  val quote = "\""

  implicit def strToStrHelper(str: String): StrHelper = StrHelper(str)


  def accumulateTokensInJson(strs: List[String]): List[String] = {
    val lBracket1 = '['
    val rBracket1 = ']'
    val lBracket2 = '{'
    val rBracket2 = '}'

    case class Acc(list: List[String], b1Counter: Int, b2Counter: Int)

    strs.foldLeft(Acc(List.empty, 0, 0)) {
      (acc, str) =>
        val lb1CountInStr = str.count(_ == lBracket1)
        val r1CountInStr = str.count(_ == rBracket1)
        val lb2CountInStr = str.count(_ == lBracket2)
        val rb2CountInStr = str.count(_ == rBracket2)

        if (acc.b1Counter > 0 || acc.b2Counter > 0) {
          val list = acc.list

          val last: String = list.last
          val newLast: String = last + "," + str
          val newList: List[String] = list.take(list.size - 1) ++ List(newLast)

          val newB1Counter = acc.b1Counter + (lb1CountInStr - r1CountInStr)
          val newB2Counter = acc.b2Counter + (lb2CountInStr - rb2CountInStr)
          Acc(newList, newB1Counter, newB2Counter)
        } else {

          val newB1Counter = acc.b1Counter + (lb1CountInStr - r1CountInStr)
          val newB2Counter = acc.b2Counter + (lb2CountInStr - rb2CountInStr)
          val newList: List[String] = acc.list ++ List(str)
          Acc(newList, newB1Counter, newB2Counter)
        }

    }.list

  }


}

package org.rurik.part1

import org.rurik.Exercise2_2.isSorted
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Exercise2_2Spec extends AnyFlatSpec with should.Matchers {

  private val intOrdered: (Int, Int) => Boolean = (i: Int, j: Int) => i < j

  "Array" should "be sorted" in {

    isSorted(Array(1, 2, 3, 4, 5), intOrdered) should be (true)

    isSorted(Array(-1,0,1, 2, 3, 4, 5,100,2000), intOrdered) should be (true)

  }

  "Array" should "not be sorted" in {

    isSorted(Array(3,2), intOrdered) should be (false)

    isSorted(Array(5,4,3,55,2,1), intOrdered) should be (false)

  }

}

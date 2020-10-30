package org.rurik.part1

import org.rurik.{Branch, Leaf, Tree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class Exercise3_25_TreeSpec extends AnyFlatSpec with should.Matchers {

  val leafTree: Tree[Int] = Leaf(1)
  val tree1: Tree[Int] = Branch(Leaf(1), Branch(Leaf(1), Leaf(6)))
  val tree2: Tree[Int] = Branch(
    Leaf(1),
    Branch(
      Leaf(1),
      Branch(Leaf(1), Leaf(6))
    )
  )

  "Tree" should "size correctly" in {
    Tree.size(leafTree) shouldBe 1
    Tree.size(tree1) shouldBe 3

    Tree.sizeF(leafTree) shouldBe 1
    Tree.sizeF(tree1) shouldBe 3
  }


  "Tree" should "maximum correctly" in {
    Tree.maximum(leafTree) shouldBe 1
    Tree.maximum(tree1) shouldBe 6

    Tree.maximumF(leafTree) shouldBe 1
    Tree.maximumF(tree1) shouldBe 6
  }

  "Tree" should "depth correctly" in {
    Tree.depth(leafTree) shouldBe 1
    Tree.depth(tree1) shouldBe 3
    Tree.depth(tree2) shouldBe 4

    Tree.depthF(leafTree) shouldBe 1
    Tree.depthF(tree1) shouldBe 3
    Tree.depthF(tree2) shouldBe 4
  }

}

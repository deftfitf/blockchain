package blockchain.core

import java.nio.charset.StandardCharsets
import java.util.Base64

import scala.util.Random

class MarkleTree(
    private[this] val N: Int,
    private[this] val tree: Array[Array[Byte]]
) {

  def markleRoot: Array[Byte] =
    tree(1).clone()

  def partialTree(transactionNumbers: List[Int]): Array[Array[Byte]] = {
    val partialTree = new Array[Array[Byte]](2 * N)

    for (transactionNumber <- transactionNumbers) {
      var idx = N + transactionNumber
      while (idx > 0) {
        if (partialTree(idx) == null) {
          partialTree(idx) = tree(idx).clone()
        }
        val leftChild = 2 * idx
        if (leftChild < 2 * N && partialTree(leftChild) == null) {
          partialTree(leftChild) = tree(leftChild).clone()
        }
        val rightChild = 2 * idx + 1
        if (rightChild < 2 * N && partialTree(rightChild) == null) {
          partialTree(rightChild) = tree(rightChild).clone()
        }
        idx /= 2
      }
    }

    partialTree
  }

  def printTree(): Unit = {
    val base64Encoded =
      tree.map(bytes =>
        if (bytes != null) {
          new String(Base64.getEncoder.encode(bytes), StandardCharsets.UTF_8)
        } else {
          "null"
        }
      )

    var bldr: scala.collection.mutable.IndexedSeq[Char] = new StringBuilder
    var l = N
    var r = 2 * N - 1
    var width = 2
    while (r > 0) {
      while (l <= r) {
        bldr = bldr.prependedAll(
          base64Encoded(r)
            .appendedAll(" " * width)
            .prependedAll(" " * width)
        )
        r -= 1
      }
      l = (r + 1) / 2
      bldr = bldr.prepended('\n')
      width *= 2
    }

    println(bldr.mkString)
  }

}

object MarkleTree {

  def construct(
      transactionHashes: Array[Array[Byte]],
      hashStrategy: HashStrategy
  ): MarkleTree = {
    val size = transactionHashes.length
    val N = Math.pow(2, (Math.log(size) / Math.log(2)).ceil).toInt
    val tree = new Array[Array[Byte]](2 * N)

    (0 until size) foreach { i =>
      tree(N + i) = transactionHashes(i).clone()
    }

    var l = N / 2
    var r = N - 1
    while (r > 0) {
      while (l <= r) {
        val concatenated =
          (Option(tree(2 * r)), Option(tree(2 * r + 1))) match {
            case (Some(l), Some(r)) => l.concat(r)
            case (Some(l), None)    => l
            case (None, Some(r))    => r
            case _                  => null
          }
        tree(r) = Option(concatenated).map(hashStrategy.hash).orNull
        r -= 1
      }
      l = (r + 1) / 2
    }

    new MarkleTree(N, tree)
  }

  def deserialize(markleTreeBytes: Array[Array[Byte]]): MarkleTree = {
    new MarkleTree(markleTreeBytes.length / 2 - 1, markleTreeBytes.clone())
  }

  def main(args: Array[String]): Unit = {
    val hashStrategy = new HashStrategy {
      override def hash(bytes: Array[Byte]): Array[Byte] = {
        Option(bytes).map(SHA256.hash).orNull
      }
    }

    val rnd = new Random()
    val bytes =
      (0 until 10).map(_ => hashStrategy.hash(rnd.nextBytes(512))).toArray
    val markleTree = MarkleTree.construct(bytes, hashStrategy)

    val partialTreeBytes = markleTree.partialTree(List(2, 4))

    val markleRoot1 = markleTree.markleRoot
    val partialTree = MarkleTree.deserialize(partialTreeBytes)
    val markleRoot2 = partialTree.markleRoot

    println("markle tree")
    markleTree.printTree()

    println("partial markle tree")
    partialTree.printTree()

    println("is same root")
    println(markleRoot1.sameElements(markleRoot2))
  }

}

package blockchain.core

object Base58 {

  private val table = Array(
    '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C',
    'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q',
    'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c',
    'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'm', 'n', 'o', 'p',
    'q', 'r', 's', 't', 'u', 'b', 'w', 'x', 'y', 'z'
  )

  def encode(bytes: Array[Byte]): String = {
    val removed0headBytes = bytes.dropWhile(_ == 0x00)
    val headRemoved = bytes.length - removed0headBytes.length

    @scala.annotation.tailrec
    def recursive(seed: BigInt, encoded: List[Char]): String =
      if (seed > 0) {
        recursive(seed / 58, table((seed % 58).intValue) :: encoded)
      } else "1" * headRemoved + encoded.mkString

    recursive(BigInt(removed0headBytes), Nil)
  }

  def decode(base58: String): Array[Byte] = {
    val removed0headBytes = base58.dropWhile(_ == '1').reverse
    val headRemoved = base58.length - removed0headBytes.length

    @scala.annotation.tailrec
    def recursive(base: BigInt, encoded: List[Char], seed: BigInt): Array[Byte] =
      encoded match {
        case h :: t =>
          val n: Long = if (h < 'A') h - '1'
          else if (h < 'I') h - 'A' + 9
          else if (h < 'O') h - 'J' + 17
          else if (h < 'a') h - 'P' + 22
          else if (h < 'l') h - 'a' + 33
          else h - 'm' + 44

          recursive(base * 58, t, seed + base * n)
        case Nil => seed.toByteArray.prependedAll(LazyList.continually[Byte](0x00).take(headRemoved).toArray)
      }

    recursive(BigInt(1), removed0headBytes.toCharArray.toList, BigInt(0))
  }

  def main(args: Array[String]): Unit = {
    val byteArray = "19g6oo8foQF5jfqK9gH2bLkFNwgCenRBPD"
    println(encode(decode(byteArray)))
  }

}

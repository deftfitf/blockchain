package blockchain.core

import java.security.MessageDigest

object SHA256 {

  private val ALGORITHM = "SHA-256"

  def doubleHash(bytes: Array[Byte]): Array[Byte] = {
    val sha256 = MessageDigest.getInstance(ALGORITHM)
    sha256.digest(sha256.digest(bytes))
  }

  def hash(bytes: Array[Byte]): Array[Byte] = {
    val sha256 = MessageDigest.getInstance(ALGORITHM)
    sha256.digest(bytes)
  }

}

package blockchain.core

import java.nio.charset.StandardCharsets
import java.security.PublicKey
import java.util.Base64

case class CoinAddress private (
    private val publicKeyHash: Array[Byte],
    version: Int,
    address: String
) {

  def getPublicKeyHash(): Array[Byte] =
    publicKeyHash.clone()

}

object CoinAddress {

  private val VERSION: Byte = 0x00

  sealed trait CoinAddressError extends Throwable
  final case class CoinAddressCheckSumError(expected: String, actual: String)
      extends CoinAddressError

  def from(publicKey: PublicKey): CoinAddress = {
    val publicKeyHash = SHA256.doubleHash(publicKey.getEncoded)
    from(publicKeyHash)
  }

  def from(publicKeyHash: Array[Byte]): CoinAddress = {
    val versioned = publicKeyHash.prepended(VERSION)
    val checkSum = SHA256.doubleHash(versioned).take(4)
    val coinAddressBytes = versioned.appendedAll(checkSum)

    val base58Encoded = Base58.encode(coinAddressBytes)
    CoinAddress(publicKeyHash, VERSION, base58Encoded)
  }

  def from(address: String): Either[CoinAddressError, CoinAddress] = {
    val base58Decoded = Base58.decode(address)

    val (versioned, expectedCheckSum) =
      base58Decoded.splitAt(base58Decoded.length - 4)
    val checkSum = SHA256.doubleHash(versioned).take(4)
    if (!expectedCheckSum.sameElements(checkSum)) {
      return Left(
        CoinAddressCheckSumError(
          new String(
            Base64.getEncoder.encode(expectedCheckSum),
            StandardCharsets.UTF_8
          ),
          new String(Base64.getEncoder.encode(checkSum), StandardCharsets.UTF_8)
        )
      )
    }
    val (version, publicKeyHash) = versioned.splitAt(1)

    Right(CoinAddress(publicKeyHash, version.apply(0), address))
  }

  def main(args: Array[String]): Unit = {
    val coinAddressString = "19g6oo8foQF5jfqK9gH2bLkFNwgCenRBPD"

    val coinAddress = CoinAddress.from(coinAddressString).getOrElse(null)
    val coinAddressFromHash = CoinAddress.from(coinAddress.publicKeyHash)

    println("is same check sum")
    println(coinAddressFromHash.address == coinAddressString)
  }

}

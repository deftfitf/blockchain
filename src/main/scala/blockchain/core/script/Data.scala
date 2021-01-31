package blockchain.core.script

trait Data extends Identifier {
  val value: Array[Byte]
}

object Data {

  case class AddressBytes(value: Array[Byte]) extends Data

  case class PKHBytes(value: Array[Byte]) extends Data

  case class PlainData(value: Array[Byte]) extends Data

  val declareToDataFactory: Map[String, Array[Byte] => Data] =
    Map(
      ("ADDR", bytes => AddressBytes(bytes)),
      ("PKH", bytes => PKHBytes(bytes))
    )

}

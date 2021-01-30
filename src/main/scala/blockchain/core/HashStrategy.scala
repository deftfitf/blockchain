package blockchain.core

trait HashStrategy {

  def hash(bytes: Array[Byte]): Array[Byte]

}

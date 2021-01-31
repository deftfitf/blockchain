package blockchain.core.script

sealed trait Operator extends Identifier {

  val code: String

  def exec(stack: List[Identifier]): List[Identifier]

}

object Operator {

  final case object OperatorDuplicate extends Operator {
    override val code: String = "OP_DUP"
    override def exec(stack: List[Identifier]): List[Identifier] =
      stack.head :: stack
  }

  final case object OperatorSHA256 extends Operator {
    override val code: String = "OP_SHA256"
//    override def exec(stack: List[Identifier]): List[Identifier] = {
//      stack.head match {
//        case data: Data =>
//          stack.head match {
//            case data: Data =>
//              CoinAddress.from(new String(data.value)) match {
//                case Right(coinAddress) =>
////                  coinAddress.getPublicKeyHash()
//                case Left(e) => throw e
//              }
//            case _ => throw new RuntimeException("data expected, but found: " + stack.head)
//          }
//        case _ => throw new RuntimeException("data expected, but found: " + stack.head)
//      }
//    }
    override def exec(stack: List[Identifier]): List[Identifier] = ???
  }

  final case object OperatorEqual extends Operator {
    override val code: String = "OP_EQUAL"
    override def exec(stack: List[Identifier]): List[Identifier] = {
      val isEqual = (stack.head, stack.tail.head) match {
        case (l: Data, r: Data) => l.equals(r)
        case _                  => false
      }
      if (isEqual) OK :: stack else stack
    }
  }

  final case object OperatorEqualVerify extends Operator {
    override val code: String = "OP_EQUALVERIFY"
    override def exec(stack: List[Identifier]): List[Identifier] = ???
  }

  final case object OperatorCheckSig extends Operator {
    override val code: String = "OP_CHECKSIG"
    override def exec(stack: List[Identifier]): List[Identifier] = ???
  }

  final case object OperatorCheckMultiSig extends Operator {
    override val code: String = "OP_CHECHMULTISIG"
    override def exec(stack: List[Identifier]): List[Identifier] = ???
  }

  val codeToOperator: Map[String, Operator] =
    List(
      OperatorDuplicate,
      OperatorSHA256,
      OperatorEqual,
      OperatorEqualVerify,
      OperatorCheckSig,
      OperatorCheckMultiSig
    ).map(op => (op.code, op))
      .toMap

}

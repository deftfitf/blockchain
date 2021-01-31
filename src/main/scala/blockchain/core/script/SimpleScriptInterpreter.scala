package blockchain.core.script

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets

import blockchain.core.script.Data.PlainData
import blockchain.core.script.SimpleScriptInterpreter.{Failed, StatusCode}

import scala.util.{Failure, Success, Using}

class SimpleScriptInterpreter {

  def interpret(is: InputStream): StatusCode = {
    Using(new BufferedReader(new InputStreamReader(is))) { buffered =>
      {
        var line = ""
        var stack: List[Identifier] = Nil
        while ({ line = buffered.readLine(); line ne null }) {
          Operator.codeToOperator.get(line) match {
            case Some(operator) => stack = operator.exec(stack)
            case None =>
              val Array(declare, data) = line.split(" ")
              val bytes = data.getBytes(StandardCharsets.UTF_8)
              val dataObj = Data.declareToDataFactory
                .get(declare)
                .map(_(bytes))
                .getOrElse(PlainData(bytes))

              stack ::= dataObj
          }
        }
        if (stack.headOption.contains(OK)) SimpleScriptInterpreter.Success
        else Failed("unknown")
      }
    } match {
      case Success(statusCode) => statusCode
      case Failure(e)          => Failed(e.getMessage)
    }
  }

}

object SimpleScriptInterpreter {

  sealed trait StatusCode
  final case object Success extends StatusCode
  final case class Failed(cause: String) extends StatusCode

}

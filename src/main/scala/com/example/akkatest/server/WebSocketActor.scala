package com.example.akkatest.server

import akka.actor.{Actor, ActorRef, Terminated}
import akka.http.scaladsl.model.ws.TextMessage
import akka.util.Timeout
import io.circe.parser.decode

import scala.concurrent.ExecutionContextExecutor

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class WebSocketActor(session: ActorRef)(implicit val dispatcher: ExecutionContextExecutor,
                                        implicit val timeout: Timeout) extends Actor with JsonUtils {

  var connection: Option[ActorRef] = None

  override def preStart(): Unit = {
    session ! ('income, self)
  }

  override def receive = {

    case TextMessage.Strict(messageText) => {
      connection.foreach { actor =>
        decode[GameRequest](messageText) match {
          case Right(request) => session ! request
          case Left(error) => actor ! toTextMessage(ErrorResponse(error.getMessage))
        }
      }
    }

    case response: GameResponse => connection.foreach { actor => actor ! toTextMessage(response)}

    case 'sinkclose =>
      println("sinkclose")
      context.stop(self)

    case ('income, a: ActorRef) => connection = Some(a)
      context.watch(a)

    case Terminated(a) if connection.contains(a) => connection = None
      println("terminated")
      context.stop(self)

    case _ => println("undefined message")
  }
}

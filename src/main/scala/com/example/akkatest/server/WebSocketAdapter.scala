package com.example.akkatest.server

import akka.actor.{Actor, ActorRef, Props, Terminated}
import akka.http.scaladsl.model.ws.TextMessage
import io.circe.parser.decode

/**
  * actor for transforming session requests and responses to web socket text messages
  *
  * @author Denis Pakhomov.
  * @version 1.0
  */
class WebSocketAdapter(session: ActorRef) extends Actor with JsonUtils {

  override def preStart(): Unit = {
    session ! ('income, self)
  }

  def notInitialized(): Receive = {
    case ('income, connection: ActorRef) =>
      context.watch(connection)
      context.become(initialized(connection))
  }

  def initialized(connection: ActorRef): Receive = {

    case TextMessage.Strict(messageText) =>
      decode[GameRequest](messageText) match {
        case Right(request) => session ! request
        case Left(error) => connection ! toTextMessage(ErrorResponse(error.getMessage))
      }

    case response: GameResponse => connection ! toTextMessage(response)

    case 'sinkclose => context.stop(self)

    case Terminated(_) => context.stop(self)

    case _ => toTextMessage(ErrorResponse("undefined message"))
  }

  override def receive() = notInitialized()
}

object WebSocketAdapter {
  def props(session: ActorRef) = Props(new WebSocketAdapter(session))
}

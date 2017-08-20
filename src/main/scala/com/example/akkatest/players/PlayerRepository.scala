package com.example.akkatest.players

import akka.actor.Actor
import com.example.akkatest.session.{GetSecret, RegisterMessage}
import RegisterResults._

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class PlayerRepository extends Actor {

  def process(users: Map[String, String]): Receive = {
    case message@RegisterMessage(_, _) =>
      val messageEither = message match {
        case _ if message.username.isEmpty => Left(InvalidName)
        case _ if message.password.isEmpty => Left(InvalidPassword)
        case _ if users.get(message.username).isDefined => Left(Exists)
        case _ => Right(message)
      }

      messageEither match {
        case Right(result) =>
          context.become(process(users + (result.username -> result.password)))
          sender() ! Registered
        case Left(registerResult) =>
          sender() ! registerResult
      }
    case GetSecret(username) => sender() ! users.get(username)
  }

  override def receive() = process(Map.empty)
}

object RegisterResults {
  sealed trait RegisterResult
  case object Registered extends RegisterResult
  case object Exists extends RegisterResult
  case object InvalidName extends RegisterResult
  case object InvalidPassword extends RegisterResult
}



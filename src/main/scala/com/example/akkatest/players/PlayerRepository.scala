package com.example.akkatest.players

import akka.actor.{Actor, Props}
import com.example.akkatest.players.PlayerRepository.{GetSecret, RegisterMessage}
import com.example.akkatest.players.RegisterResults._

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class PlayerRepository extends Actor {

  def process(users: Map[String, String]): Receive = {
    case message @ RegisterMessage(_, _) =>
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

object PlayerRepository {
  def props() = Props[PlayerRepository]

  case class RegisterMessage(username: String, password: String)
  case class GetSecret(username: String)
}

object RegisterResults {
  sealed trait RegisterResult
  case object Registered extends RegisterResult
  case object Exists extends RegisterResult
  case object InvalidName extends RegisterResult
  case object InvalidPassword extends RegisterResult
}



package com.example.akkatest.session

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.example.akkatest.session.LoginResults._

import scala.concurrent.ExecutionContextExecutor
import scala.util.Success

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class ServerGateway(playerRepository: ActorRef, matchMaking: ActorRef)
                   (implicit val dispatcher: ExecutionContextExecutor, implicit val timeout: Timeout) extends Actor {

  def receiveFunc(sessionIds: Map[String, UUID]): Receive = {
    case message @ RegisterMessage(_, _) => playerRepository.forward(message)

    case message @ GetSecret(_) => playerRepository.forward(message)

    case StartSession() =>
      val sessionActor = context.actorOf(Props(new Session(UUID.randomUUID(), matchMaking)))
      println(sessionActor)
      sender() ! sessionActor

    case LoginMessage(id, username, password) =>
      val reply = sender()
      sessionIds.get(username) match {
        case Some(_) => reply ! AlreadyLogin

        case None => playerRepository.ask(GetSecret(username)).mapTo[Option[String]].map {
          case None => UserNotExists
          case Some(storedPassword) if storedPassword != password => IncorrectPassword
          case _ =>
            context.become(receiveFunc(sessionIds + (username -> id)))
            Successful
        }.onComplete {
          case Success(futureResult) => reply ! futureResult
          case _ => reply ! PersistenceError
        }
      }

    case _ => None
  }

  def receive = receiveFunc(Map.empty)
}

object LoginResults {
  sealed trait LoginResult
  case object AlreadyLogin extends LoginResult
  case object IncorrectPassword extends LoginResult
  case object UserNotExists extends LoginResult
  case object PersistenceError extends LoginResult
  case object Successful extends LoginResult
}

case class RegisterMessage(username: String, password: String)
case class LoginMessage(sessionId: UUID, username: String, password: String)
case class StartSession()
case class GetSession(uuid: UUID)
case class GetSecret(username: String)
case class UserRegistered(username: String)

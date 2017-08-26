package com.example.akkatest.session

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.example.akkatest.session.ServerGateway.LoginResults._
import com.example.akkatest.session.ServerGateway.{GetSecret, LoginMessage, RegisterMessage, StartSession}

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
      val sessionActor = context.actorOf(Session.props(UUID.randomUUID(), matchMaking))
      println(sessionActor)
      sender() ! sessionActor

    case LoginMessage(id, username, password) =>
      val reply = sender()
      sessionIds.get(username) match {
        case Some(_) => reply ! AlreadyLogin(username)

        case None => playerRepository.ask(GetSecret(username)).mapTo[Option[String]].map {
          case None => UserNotExists(username)
          case Some(storedPassword) if storedPassword != password => IncorrectPassword(username)
          case _ =>
            context.become(receiveFunc(sessionIds + (username -> id)))
            Successful(username)
        }.onComplete {
          case Success(futureResult) => reply ! futureResult
          case _ => reply ! PersistenceError
        }
      }

    case _ => None
  }

  def receive = receiveFunc(Map.empty)
}

object ServerGateway {

  def props(playerRepository: ActorRef, matchMaking: ActorRef)
           (implicit dispatcher: ExecutionContextExecutor, timeout: Timeout) =
    Props(new ServerGateway(playerRepository, matchMaking))

  object LoginResults {
    sealed trait LoginResult
    case class AlreadyLogin(username: String) extends LoginResult
    case class IncorrectPassword(username: String) extends LoginResult
    case class UserNotExists(username: String) extends LoginResult
    case object PersistenceError extends LoginResult
    case class Successful(username: String) extends LoginResult
  }

  case class RegisterMessage(username: String, password: String)
  case class LoginMessage(sessionId: UUID, username: String, password: String)
  case class StartSession()
  case class GetSession(uuid: UUID)
  case class GetSecret(username: String)
  case class UserRegistered(username: String)
}

package com.example.akkatest.session

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.example.akkatest.players.PlayerRepository.GetSecret
import com.example.akkatest.session.SessionRepository.LoginResults._
import com.example.akkatest.session.SessionRepository.{LoginMessage, StartSession}

import scala.concurrent.ExecutionContextExecutor
import scala.util.Success

/**
  * Service actor for controlling player sessions
  *
  * @author Denis Pakhomov.
  * @version 1.0
  */
class SessionRepository(sessions: Map[String, UUID], gateway: ActorRef)
                       (implicit val dispatcher: ExecutionContextExecutor,
                        implicit val timeout: Timeout) extends Actor {

  def this(gateway: ActorRef)(implicit dispatcher: ExecutionContextExecutor, timeout: Timeout) =
    this(Map.empty, gateway)

  def receiveFunc(sessions: Map[String, UUID]): Receive = {
    case StartSession() =>
      val sessionActor = context.actorOf(Session.props(UUID.randomUUID(), gateway))
      sender() ! sessionActor

    case LoginMessage(id, username, password) =>
      val reply = sender()
      sessions.get(username) match {
        case Some(_) => reply ! AlreadyLogin(username)

        case None => gateway.ask(GetSecret(username)).mapTo[Option[String]].map {
          case None => UserNotExists(username)
          case Some(storedPassword) if storedPassword != password => IncorrectPassword(username)
          case _ =>
            context.become(receiveFunc(sessions + (username -> id)))
            Successful(username)
        }.onComplete {
          case Success(futureResult) => reply ! futureResult
          case _ => reply ! PersistenceError
        }
      }
  }

  override def receive: Receive = receiveFunc(sessions)
}

object SessionRepository {
  def props(users: Map[String, UUID], gateway: ActorRef)(implicit dispatcher: ExecutionContextExecutor,
                                                         timeout: Timeout) = Props(new SessionRepository(users, gateway))
  def props(gateway: ActorRef)(implicit dispatcher: ExecutionContextExecutor,
                               timeout: Timeout) = Props(new SessionRepository(gateway))

  object LoginResults {
    sealed trait LoginResult
    case class AlreadyLogin(username: String) extends LoginResult
    case class IncorrectPassword(username: String) extends LoginResult
    case class UserNotExists(username: String) extends LoginResult
    case object PersistenceError extends LoginResult
    case class Successful(username: String) extends LoginResult
  }

  case class LoginMessage(sessionId: UUID, username: String, password: String)
  case class StartSession()
}

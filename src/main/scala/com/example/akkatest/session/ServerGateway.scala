package com.example.akkatest.session

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.example.akkatest.game.GameManagerActor
import com.example.akkatest.game.GameManagerActor.CreateGame
import com.example.akkatest.matchmaking.{MatchMakingActor, MatchPlayers}
import com.example.akkatest.players.PlayerRepository
import com.example.akkatest.server.GetOpponents
import com.example.akkatest.session.ServerGateway.LoginResults._
import com.example.akkatest.session.ServerGateway.{GetSecret, LoginMessage, RegisterMessage, StartSession}
import com.example.akkatest.session.Session.AddToMatching

import scala.concurrent.ExecutionContextExecutor
import scala.util.Success

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class ServerGateway()(implicit val dispatcher: ExecutionContextExecutor, implicit val timeout: Timeout) extends Actor {

  private val playerRepository = context.actorOf(PlayerRepository.props())
  private val matchMaking = context.actorOf(MatchMakingActor.props(context.self))
  private val gameManager = context.actorOf(GameManagerActor.props())

  def receiveFunc(sessionIds: Map[String, UUID]): Receive = {
    case message @ RegisterMessage(_, _) => playerRepository.forward(message)

    case message @ GetSecret(_) => playerRepository.forward(message)

    case StartSession() =>
      val sessionActor = context.actorOf(Session.props(UUID.randomUUID(), context.self))
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

    case req @ AddToMatching(_, _) => matchMaking.forward(req)
    case req @ GetOpponents() => matchMaking.forward(req)
    case req @ MatchPlayers(_, _) => matchMaking.forward(req)

    case req @ CreateGame(_, _) => gameManager ! req

    case _ => None
  }

  def receive = receiveFunc(Map.empty)
}

object ServerGateway {

  def props()(implicit dispatcher: ExecutionContextExecutor, timeout: Timeout) = Props(new ServerGateway())

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

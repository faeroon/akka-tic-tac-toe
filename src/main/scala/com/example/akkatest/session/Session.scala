package com.example.akkatest.session

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import com.example.akkatest.game.{GameError, GameResult}
import com.example.akkatest.matchmaking.MatchMakingStatuses.{AddedToMatch, NotMatched}
import com.example.akkatest.matchmaking.{MatchEnded, MatchPlayers}
import com.example.akkatest.players.RegisterResults.{RegisterResult, Registered}
import com.example.akkatest.server._
import com.example.akkatest.session.ServerGateway.LoginResults.{LoginResult, Successful}
import com.example.akkatest.session.ServerGateway.{LoginMessage, RegisterMessage}
import com.example.akkatest.session.Session.AddToMatching

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class Session(id: UUID, matchMaking: ActorRef) extends Actor {

  def notInitialized(): Receive = {
    case ('income, socket: ActorRef) => context.become(anonymous(socket))
  }

  //region anonymous state

  def anonymous(socket: ActorRef): Receive = {
    case Register(username, password) =>
      if (sender() == socket) context.parent ! RegisterMessage(username, password)
      else sender() ! Error("invalid sender")

    case Registered => socket ! Ok()

    case error: RegisterResult => socket ! Error(error.toString)

    case Login(username, password) =>
      if (sender() == socket) context.parent ! LoginMessage(id, username, password)
      else sender() ! Error("invalid sender")

    case Successful(username) =>
      context.become(authorized(socket, username))
      socket ! Ok()

    case error: LoginResult => socket ! Error(error.toString)

    case _ => sender() ! Error("invalid state for request")
  }

  //endregion

  //region authorized state

  def authorized(socket: ActorRef, username: String): Receive =  {

    case ReadyToMatch() => matchMaking ! AddToMatching(username, context.self)

    case NotMatched => socket ! Error(NotMatched.toString)

    case AddedToMatch =>
      context.become(online(socket, username))
      socket ! Ok()

    case _ => sender() ! Error("invalid state for request")
  }

  //endregion

  //region online state

  def online(socket: ActorRef, username: String): Receive = {

    case request @ GetOpponents() => matchMaking ! request
    case response @ OpponentsList(_) => socket ! response

    case MatchWith(opponent) => matchMaking ! MatchPlayers(username, opponent)

    case NotMatched => socket ! Error(NotMatched.toString)

    case resp @ GameStarted(_) =>
      println(resp)
      context.become(matching(socket, username, sender()))
      socket ! resp

    case _ => sender() ! Error("invalid state for request")
  }

  //endregion

  //region matching state

  def matching(socket: ActorRef, username: String, game: ActorRef): Receive = {
    case error: GameError => socket ! Error(error.toString)
    case resp @ PlayerMadeMove(_, _, _, _) => socket ! resp
    case req @ MakeMove(_, _) => game ! req
    case resp : GameResult =>
      socket ! resp
      matchMaking ! MatchEnded(username)
      context.become(online(socket, username))

    case _ => sender() ! Error("invalid state for request")
  }

  //endregion

  override def receive = notInitialized()
}

object Session {

  def props(id: UUID, matchmakingActor: ActorRef) = Props(new Session(id, matchmakingActor))

  case class AddToMatching(username: String, session: ActorRef)
}

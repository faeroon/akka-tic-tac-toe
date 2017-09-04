package com.example.akkatest.session

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import com.example.akkatest.game.{GameError, GameResult}
import com.example.akkatest.matchmaking.MatchMakingStatuses.{AddedToMatch, NotMatched}
import com.example.akkatest.matchmaking.{MatchEnded, MatchPlayers}
import com.example.akkatest.players.PlayerRepository.RegisterMessage
import com.example.akkatest.players.RegisterResults.{RegisterResult, Registered}
import com.example.akkatest.server._
import com.example.akkatest.session.Session.AddToMatching
import com.example.akkatest.session.SessionRepository.LoginMessage
import com.example.akkatest.session.SessionRepository.LoginResults.{LoginResult, Successful}

/**
  * Player game session
  *
  * @author Denis Pakhomov.
  * @version 1.0
  */
class Session(id: UUID, gateway: ActorRef) extends Actor {

  /**
    * state when it's not connected to web socket actor. After connection state changes to "anonymous"
    *
    * @return
    */
  def notInitialized(): Receive = {
    case ('income, socket: ActorRef) => context.become(anonymous(socket))
  }

  //region anonymous state

  /**
    * Anonymous player state. In this state player can create new account and login.
    * After successful authorization state changes to "authorized"
    *
    * @param socket
    * @return
    */
  def anonymous(socket: ActorRef): Receive = {
    case Register(username, password) =>
      if (sender() == socket) gateway ! RegisterMessage(username, password)
      else sender() ! Error("invalid sender")

    case Registered => socket ! Ok()

    case error: RegisterResult => socket ! Error(error.toString)

    case Login(username, password) =>
      if (sender() == socket) gateway ! LoginMessage(id, username, password)
      else sender() ! Error("invalid sender")

    case Successful(username) =>
      context.become(authorized(socket, username))
      socket ! Ok()

    case error: LoginResult => socket ! Error(error.toString)

    case _ => sender() ! Error("invalid state for request")
  }

  //endregion

  //region authorized state

  /**
    * State for authorized player. In this state player is disabled for matching and isn't showed in opponents list
    *
    * @param socket
    * @param username
    * @return
    */
  def authorized(socket: ActorRef, username: String): Receive =  {

    case ReadyToMatch() => gateway ! AddToMatching(username, context.self)

    case NotMatched => socket ! Error(NotMatched.toString)

    case AddedToMatch =>
      context.become(online(socket, username))
      socket ! Ok()

    case _ => sender() ! Error("invalid state for request")
  }

  //endregion

  //region online state

  /**
    * State for player ready to game. Player can start new tic-tac-toe game in this state
    *
    * @param socket
    * @param username
    * @return
    */
  def online(socket: ActorRef, username: String): Receive = {

    case request @ GetOpponents() => gateway ! request
    case response @ OpponentsList(_) => socket ! response

    case MatchWith(opponent) => gateway ! MatchPlayers(username, opponent)

    case NotMatched => socket ! Error(NotMatched.toString)

    case resp @ GameStarted(_) =>
      context.become(matching(socket, username, sender()))
      socket ! resp

    case _ => sender() ! Error("invalid state for request")
  }

  //endregion

  //region matching state

  /**
    * State for game match. In this state players can send their moves to game actor.
    * After game end state changes to "authorized"
    *
    * @param socket
    * @param username
    * @param game
    * @return
    */
  def matching(socket: ActorRef, username: String, game: ActorRef): Receive = {
    case error: GameError => socket ! Error(error.toString)
    case resp @ PlayerMadeMove(_, _, _, _) => socket ! resp
    case req @ MakeMove(_, _) => game ! req
    case resp : GameOver =>
      socket ! resp
      gateway ! MatchEnded(username)
      context.become(authorized(socket, username))

    case _ => sender() ! Error("invalid state for request")
  }

  //endregion

  override def receive = notInitialized()
}

object Session {

  def props(id: UUID, gateway: ActorRef) = Props(new Session(id, gateway))

  case class AddToMatching(username: String, session: ActorRef)
}

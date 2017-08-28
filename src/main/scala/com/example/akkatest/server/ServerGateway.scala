package com.example.akkatest.server

import akka.actor.{Actor, Props}
import akka.util.Timeout
import com.example.akkatest.game.GameManagerActor
import com.example.akkatest.game.GameManagerActor.CreateGame
import com.example.akkatest.matchmaking.{MatchMakingActor, MatchPlayers}
import com.example.akkatest.players.PlayerRepository
import com.example.akkatest.players.PlayerRepository.{GetSecret, RegisterMessage}
import com.example.akkatest.session.Session.AddToMatching
import com.example.akkatest.session.SessionRepository
import com.example.akkatest.session.SessionRepository.{LoginMessage, StartSession}

import scala.concurrent.ExecutionContextExecutor

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class ServerGateway()(implicit val dispatcher: ExecutionContextExecutor, implicit val timeout: Timeout) extends Actor {

  private val playerRepository = context.actorOf(PlayerRepository.props())
  private val matchMaking = context.actorOf(MatchMakingActor.props(context.self))
  private val gameManager = context.actorOf(GameManagerActor.props())
  private val sessionRepository = context.actorOf(SessionRepository.props(context.self))

  override def receive(): Receive = {

    case message @ RegisterMessage(_, _) => playerRepository.forward(message)
    case message @ GetSecret(_) => playerRepository.forward(message)

    case message @ StartSession() => sessionRepository.forward(message)
    case message @ LoginMessage(_, _, _) => sessionRepository.forward(message)

    case message @ AddToMatching(_, _) => matchMaking.forward(message)
    case message @ GetOpponents() => matchMaking.forward(message)
    case message @ MatchPlayers(_, _) => matchMaking.forward(message)

    case message @ CreateGame(_, _) => gameManager ! message

    case _ => None
  }
}

object ServerGateway {
  def props()(implicit dispatcher: ExecutionContextExecutor, timeout: Timeout) = Props(new ServerGateway())
}

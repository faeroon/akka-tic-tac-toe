package com.example.akkatest.game

import akka.actor.{Actor, Props}
import com.example.akkatest.game.GameManagerActor.CreateGame

/**
  * Supervisor actor for game actors
  *
  * @author Denis Pakhomov.
  * @version 1.0
  */
class GameManagerActor() extends Actor {
  def receive = {
    case CreateGame(player1, player2) => context.actorOf(GameActor.props(player1, player2))
  }
}

object GameManagerActor {
  def props() = Props[GameManagerActor]

  case class CreateGame(player1: PlayerInfo, player2: PlayerInfo)
}

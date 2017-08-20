package com.example.akkatest.game

import akka.actor.{Actor, Props}

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class GameManagerActor() extends Actor {
  def receive = {
    case CreateGame(player1, player2) =>
      context.actorOf(Props(new GameActor(player1, player2)))
  }
}

case class CreateGame(player1: PlayerInfo, player2: PlayerInfo)

package com.example.akkatest.game

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import com.example.akkatest.server._

/**
  * Actor controlling move messages to tick-tack-toe game
  *
  * @author Denis Pakhomov.
  * @version 1.0
  */
class GameActor(game: TicTacToeGame, player1: PlayerInfo, player2: PlayerInfo) extends Actor {
  def this(player1: PlayerInfo, player2: PlayerInfo) {this (new TicTacToeGame(), player1, player2)}

  private val players: Map[ActorRef, PlayerRecord] = Map(
    (player1.actor, PlayerRecord(player1.username, player2.username, X)),
    (player2.actor, PlayerRecord(player2.username, player1.username, O))
  )

  def process(game: TicTacToeGame): Receive = {
    case MakeMove(x, y) =>
      players.get(sender()) match {
        case Some(player) =>
          game.makeMove(Move(player.mark, x, y)) match {
            case Left(error) => sender() ! error

            case Right(state) =>

              players.keys.foreach(actor => actor ! PlayerMadeMove(player.username, x, y, player.mark.toString))

              state.result match {
                case Some(Win(mark)) =>

                  val pair = if (mark == X) (player1.actor, player2.actor) else (player2.actor, player1.actor)
                  pair._1 ! com.example.akkatest.server.Win()
                  pair._2 ! Loss()

                  context.self ! PoisonPill

                case Some(Draw) =>
                  players.keys.foreach(player => player ! com.example.akkatest.server.Draw())

                  context.self ! PoisonPill

                case _ => None
              }
              context.become(process(state))
        }
        case None => sender() ! Error("Invalid player")
      }

    case _ => None
  }

  override def preStart(): Unit = {
    players.foreach {case (actor, player) => actor ! GameStarted(player.opponent)}
  }

  override def receive = process(game)
}

object GameActor {
  def props(player1: PlayerInfo, player2: PlayerInfo) = Props(new GameActor(player1, player2))
  def props(game: TicTacToeGame, player1: PlayerInfo, player2: PlayerInfo) = Props(new GameActor(game, player1, player2))
}

case class PlayerInfo(actor: ActorRef, username: String)

case class PlayerRecord(username: String, opponent: String, mark: Mark)

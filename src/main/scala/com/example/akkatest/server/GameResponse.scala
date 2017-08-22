package com.example.akkatest.server

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
sealed abstract class GameResponse

sealed abstract class GameOver extends GameResponse
case class Win() extends GameOver
case class Loss() extends GameOver
case class Draw() extends GameOver

case class PlayerMadeMove(player: String, x: Int, y: Int, figure: String) extends GameResponse
case class GameStarted(opponent: String) extends GameResponse

case class Ok() extends GameResponse
case class OpponentsList(opponents: Vector[String]) extends GameResponse
case class Error(message: String) extends GameResponse
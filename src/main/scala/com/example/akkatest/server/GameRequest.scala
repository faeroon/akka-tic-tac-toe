package com.example.akkatest.server

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
sealed abstract class GameRequest

case class MakeMove(x: Int, y: Int) extends GameRequest

case class ReadyToMatchRequest() extends GameRequest
case class GetOpponentsRequest() extends GameRequest
case class MatchWithRequest(user: String) extends GameRequest

case class RegisterRequest(username: String, password: String) extends GameRequest
case class LoginRequest(username: String, password: String) extends GameRequest

package com.example.akkatest.server

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
sealed abstract class GameRequest

case class MakeMove(x: Int, y: Int) extends GameRequest

case class ReadyToMatch() extends GameRequest
case class GetOpponents() extends GameRequest
case class MatchWith(user: String) extends GameRequest

case class Register(username: String, password: String) extends GameRequest
case class Login(username: String, password: String) extends GameRequest

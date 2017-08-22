package com.example.akkatest.session

import java.util.UUID

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.example.akkatest.game.{GameError, GameResult}
import com.example.akkatest.matchmaking.{MatchEnded, MatchPlayers}
import com.example.akkatest.players.RegisterResults.{RegisterResult, Registered}
import com.example.akkatest.server._
import com.example.akkatest.session.ServerGateway.LoginResults.{LoginResult, Successful}
import com.example.akkatest.session.ServerGateway.{LoginMessage, RegisterMessage}
import com.example.akkatest.session.Session.ReadyToMatch

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class Session(id: UUID, matchMaking: ActorRef)(implicit val dispatcher: ExecutionContextExecutor,
                                               implicit val timeout: Timeout) extends Actor {

  def notInitialized(): Receive = {
    case ('income, socket: ActorRef) => context.become(anonymous(socket))
  }

  //region anonymous state

  def anonymous(socket: ActorRef): Receive = {
    case RegisterRequest(username, password) =>
      val reply = sender()
      context.parent.ask(RegisterMessage(username, password)).mapTo[RegisterResult].onComplete {

        case Success(result) => result match {

          case Registered => reply ! Ok()
          case error: RegisterResult => reply ! ErrorResponse(error.toString)
        }

        case Failure(exception) => reply ! ErrorResponse(exception.getMessage)
      }

    case LoginRequest(username, password) =>
      val reply = sender()
      context.parent.ask(LoginMessage(id, username, password)).mapTo[LoginResult].onComplete {
        case Success(result) => result match {
          case Successful =>
            context.become(authorized(socket, username))
            reply ! Ok()
          case res: LoginResult => reply ! ErrorResponse(res.toString)
        }
        case Failure(error) => reply ! ErrorResponse(error.toString)
      }

    case _ => sender() ! ErrorResponse("invalid state for request")
  }

  //endregion

  //region authorized state

  def authorized(socket: ActorRef, username: String): Receive =  {
    case ReadyToMatchRequest() =>
      val reply = sender()
      matchMaking.ask(ReadyToMatch(username, context.self)).mapTo[Boolean].onComplete {
        case Success(result) => if (result) {
          context.become(online(socket, username))
        }
          reply ! (if (result) Ok() else ErrorResponse("user is in match"))
        case Failure(_) => reply ! ErrorResponse("future")
      }

    case _ => sender() ! ErrorResponse("invalid state for request")
  }

  //endregion

  //region online state

  def online(socket: ActorRef, username: String): Receive = {
    //TODO fix bug with sender() checking
    case request @ GetOpponentsRequest() => matchMaking.forward(request)

    case MatchWithRequest(opponent) =>
      val reply = sender()
      matchMaking.ask(MatchPlayers(username, opponent)).mapTo[Boolean].onComplete {
        case Success(result) => if (result) {
          reply ! Ok()
        } else
          reply ! ErrorResponse("can't match")

        case Failure(exception) => reply ! ErrorResponse(exception.toString)
      }

    case resp @ GameStarted(_) =>
      println(resp)
      context.become(matching(socket, username, sender()))
      socket ! resp

    case _ => sender() ! ErrorResponse("invalid state for request")
  }

  //endregion

  //region matching state

  def matching(socket: ActorRef, username: String, game: ActorRef): Receive = {


    case error: GameError => socket ! ErrorResponse(error.toString)
    case resp @ PlayerMadeMove(_, _, _, _) => socket ! resp
    case req @ MakeMove(_, _) => game ! req
    case resp : GameResult =>
      socket ! resp
      matchMaking ! MatchEnded(username)
      context.become(online(socket, username))

    case _ => sender() ! ErrorResponse("invalid state for request")
  }

  //endregion

  override def receive = notInitialized()
}

object Session {

  def props(id: UUID, matchmakingActor: ActorRef)(implicit dispatcher: ExecutionContextExecutor, timeout: Timeout) =
    Props(new Session(id, matchmakingActor))

  case class ReadyToMatch(username: String, session: ActorRef)
}

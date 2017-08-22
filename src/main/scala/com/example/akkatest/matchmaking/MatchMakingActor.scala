package com.example.akkatest.matchmaking

import akka.actor.{Actor, ActorRef, Props}
import com.example.akkatest.game.GameManagerActor.CreateGame
import com.example.akkatest.game.PlayerInfo
import com.example.akkatest.matchmaking.MatchMakingStatuses.{Available, InMatch, MatchMakingStatus}
import com.example.akkatest.server.{GetOpponentsRequest, OpponentsListResponse}
import com.example.akkatest.session.Session.ReadyToMatch

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class MatchMakingActor(users: Map[String, MatchmakingRecord], gameManager: ActorRef) extends Actor {

  def this(gameManager: ActorRef) { this(Map.empty, gameManager) }

  def process(users: Map[String, MatchmakingRecord]): Receive = {

    case ReadyToMatch(username, session) =>
      users.get(username) match {
      case Some(record) => sender() ! (record.status == Available)
      case None => context.become(process(users + (username -> MatchmakingRecord(session, Available))))
        sender() ! true
    }

    case GetOpponentsRequest() => sender() !
      OpponentsListResponse( users.filter {case (_, record) => record.actor != sender() && record.status == Available }.keys.toVector)

    case MatchPlayers(user1, user2) =>

      val usernames = Seq(user1, user2)
      if (user1 != user2 && usernames.forall(username => users.get(username).map(rec => rec.status).contains(Available))) {
        sender() ! true
        context.become(process(users ++ usernames.map(username => (username, users(username).copy(status = InMatch))).toMap))
        gameManager ! CreateGame(PlayerInfo(users(user1).actor, user1), PlayerInfo(users(user2).actor, user2))
      } else sender() ! false

    case MatchEnded(user) =>
      context.become(process(users - user))

    case "players" => sender() ! users

    case _ => None
  }

  def receive = process(users)
}

object MatchMakingActor {
  def props(users: Map[String, MatchmakingRecord], gameManager: ActorRef) =
    Props(new MatchMakingActor(users, gameManager))

  def props(gameManager: ActorRef) = Props(new MatchMakingActor(gameManager))
}

case class MatchPlayers(user1: String, user2: String)
case class MatchEnded(username: String)

object MatchMakingStatuses {
  sealed trait MatchMakingStatus
  case object Available extends MatchMakingStatus
  case object InMatch extends MatchMakingStatus
}

case class MatchmakingRecord(actor: ActorRef, status: MatchMakingStatus)

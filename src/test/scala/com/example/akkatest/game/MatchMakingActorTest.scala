package com.example.akkatest.game

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.example.akkatest.game.GameManagerActor.CreateGame
import com.example.akkatest.matchmaking.MatchMakingStatuses._
import com.example.akkatest.matchmaking.{MatchEnded, MatchMakingActor, MatchPlayers, MatchmakingRecord}
import com.example.akkatest.server.{GetOpponents, OpponentsList}
import com.example.akkatest.session.Session.AddToMatching
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class MatchMakingActorTest extends TestKit(ActorSystem("testSystem"))
  with ImplicitSender
  with WordSpecLike
  with MustMatchers
  with BeforeAndAfterAll {
  "a match making actor" must {
    "add new players with status Available" in {

      val gateway = TestProbe()
      val matchMakingActor = system.actorOf(MatchMakingActor.props(gateway.ref))

      val player1Probe = TestProbe()

      matchMakingActor ! AddToMatching("user1", player1Probe.ref)
      expectMsg(AddedToMatch)

      matchMakingActor ! "players"
      expectMsg(Map("user1" -> MatchmakingRecord(player1Probe.ref, Available)))
    }
    "make no changes if user is already ready to match" in {

      val gateway = TestProbe()
      val player1Probe = TestProbe()

      val players = Map("user1" -> MatchmakingRecord(player1Probe.ref, Available))

      val matchMakingActor = system.actorOf(MatchMakingActor.props(players, gateway.ref))

      matchMakingActor ! AddToMatching("user1", player1Probe.ref)
      expectMsg(AddedToMatch)

      matchMakingActor ! "players"
      expectMsg(players)
    }
    "forbid adding to matchmaking players in match" in {

      val gateway = TestProbe()
      val player1Probe = TestProbe()

      val players = Map("user1" -> MatchmakingRecord(player1Probe.ref, InMatch))

      val matchMakingActor = system.actorOf(MatchMakingActor.props(players, gateway.ref))

      matchMakingActor ! AddToMatching("user1", player1Probe.ref)
      expectMsg(PlayerIsInMatch)

      matchMakingActor ! "players"
      expectMsg(players)
    }

    "get opponents list returns all active players" in {

      val gateway = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()
      val player3Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available),
        "user3" -> MatchmakingRecord(player3Probe.ref, InMatch)
      )

      val matchMakingActor = system.actorOf(MatchMakingActor.props(players, gateway.ref))

      matchMakingActor.tell(GetOpponents(), player1Probe.ref)
      player1Probe.expectMsg(OpponentsList(Vector("user2")))
    }

    "matching available players" in {

      val gateway = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)
      )

      val matchMakingActor = system.actorOf(MatchMakingActor.props(players, gateway.ref))

      matchMakingActor.tell(MatchPlayers("user1", "user2"), player1Probe.ref)

      matchMakingActor ! "players"
      expectMsg(Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, InMatch),
        "user2" -> MatchmakingRecord(player2Probe.ref, InMatch)))

      gateway.expectMsg(CreateGame(PlayerInfo(player1Probe.ref, "user1"),
        PlayerInfo(player2Probe.ref, "user2")))
    }

    "doesn't match matched players" in {

      val gateway = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, InMatch)
      )

      val matchMakingActor = system.actorOf(MatchMakingActor.props(players, gateway.ref))

      matchMakingActor.tell(MatchPlayers("user1", "user2"), player1Probe.ref)
      player1Probe.expectMsg(NotMatched)

      matchMakingActor ! "players"
      expectMsg(Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, InMatch)))
    }

    "doesn't match not ready to match players" in {

      val gateway = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)
      )

      val matchMakingActor = system.actorOf(MatchMakingActor.props(players, gateway.ref))

      matchMakingActor.tell(MatchPlayers("user1", "user3"), player1Probe.ref)
      player1Probe.expectMsg(NotMatched)

      matchMakingActor ! "players"
      expectMsg(Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)))
    }

    "doesn't match player with themself" in {

      val gateway = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)
      )

      val matchMakingActor = system.actorOf(MatchMakingActor.props(players, gateway.ref))

      matchMakingActor.tell(MatchPlayers("user1", "user1"), player1Probe.ref)
      player1Probe.expectMsg(NotMatched)

      matchMakingActor ! "players"
      expectMsg(Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)))
    }

    "removes player from match making then match ended" in {

      val gateway = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, InMatch),
        "user2" -> MatchmakingRecord(player2Probe.ref, InMatch)
      )

      val matchMakingActor = system.actorOf(MatchMakingActor.props(players, gateway.ref))

      matchMakingActor ! MatchEnded("user1")

      matchMakingActor ! "players"
      expectMsg(Map("user2" -> MatchmakingRecord(player2Probe.ref, InMatch)))
    }
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }
}

package com.example.akkatest.game

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.example.akkatest.matchmaking.MatchMakingStatuses.{Available, InMatch}
import com.example.akkatest.matchmaking.{MatchEnded, MatchMakingActor, MatchPlayers, MatchmakingRecord}
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}
import com.example.akkatest.server.{GetOpponentsRequest, OpponentsListResponse}
import com.example.akkatest.session.ReadyToMatch

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

      val gameManagerActor = TestProbe()
      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(gameManagerActor.ref)))

      val player1Probe = TestProbe()

      matchMakingActor ! ReadyToMatch("user1", player1Probe.ref)
      expectMsg(true)

      matchMakingActor ! "players"
      expectMsg(Map("user1" -> MatchmakingRecord(player1Probe.ref, Available)))
    }
    "make no changes if user is already ready to match" in {

      val gameManagerActor = TestProbe()
      val player1Probe = TestProbe()

      val players = Map("user1" -> MatchmakingRecord(player1Probe.ref, Available))

      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(players, gameManagerActor.ref)))

      matchMakingActor ! ReadyToMatch("user1", player1Probe.ref)
      expectMsg(true)

      matchMakingActor ! "players"
      expectMsg(players)
    }
    "forbid adding to matchmaking players in match" in {

      val gameManagerActor = TestProbe()
      val player1Probe = TestProbe()

      val players = Map("user1" -> MatchmakingRecord(player1Probe.ref, InMatch))

      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(players, gameManagerActor.ref)))

      matchMakingActor ! ReadyToMatch("user1", player1Probe.ref)
      expectMsg(false)

      matchMakingActor ! "players"
      expectMsg(players)
    }

    "get opponents list returns all active players" in {

      val gameManagerActor = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()
      val player3Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available),
        "user3" -> MatchmakingRecord(player3Probe.ref, InMatch)
      )

      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(players, gameManagerActor.ref)))

      matchMakingActor.tell(GetOpponentsRequest(), player1Probe.ref)
      player1Probe.expectMsg(OpponentsListResponse(Vector("user2")))
    }

    "matching available players" in {

      val gameManagerActor = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)
      )

      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(players, gameManagerActor.ref)))

      matchMakingActor.tell(MatchPlayers("user1", "user2"), player1Probe.ref)
      player1Probe.expectMsg(true)

      matchMakingActor ! "players"
      expectMsg(Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, InMatch),
        "user2" -> MatchmakingRecord(player2Probe.ref, InMatch)))
    }

    "doesn't match matched players" in {

      val gameManagerActor = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, InMatch)
      )

      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(players, gameManagerActor.ref)))

      matchMakingActor.tell(MatchPlayers("user1", "user2"), player1Probe.ref)
      player1Probe.expectMsg(false)

      matchMakingActor ! "players"
      expectMsg(Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, InMatch)))
    }

    "doesn't match not ready to match players" in {

      val gameManagerActor = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)
      )

      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(players, gameManagerActor.ref)))

      matchMakingActor.tell(MatchPlayers("user1", "user3"), player1Probe.ref)
      player1Probe.expectMsg(false)

      matchMakingActor ! "players"
      expectMsg(Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)))
    }

    "doesn't match player with themself" in {

      val gameManagerActor = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)
      )

      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(players, gameManagerActor.ref)))

      matchMakingActor.tell(MatchPlayers("user1", "user1"), player1Probe.ref)
      player1Probe.expectMsg(false)

      matchMakingActor ! "players"
      expectMsg(Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, Available),
        "user2" -> MatchmakingRecord(player2Probe.ref, Available)))
    }

    "removes player from match making then match ended" in {

      val gameManagerActor = TestProbe()
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()

      val players = Map(
        "user1" -> MatchmakingRecord(player1Probe.ref, InMatch),
        "user2" -> MatchmakingRecord(player2Probe.ref, InMatch)
      )

      val matchMakingActor = system.actorOf(Props(new MatchMakingActor(players, gameManagerActor.ref)))

      matchMakingActor ! MatchEnded("user1")

      matchMakingActor ! "players"
      expectMsg(Map("user2" -> MatchmakingRecord(player2Probe.ref, InMatch)))
    }
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }
}

package com.example.akkatest.game

import akka.actor._
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}
import com.example.akkatest.server._

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class GameActorTest extends TestKit(ActorSystem("testSystem"))
  with ImplicitSender
  with WordSpecLike
  with MustMatchers
  with BeforeAndAfterAll {
  "A game actor" must {
    "send player moved" in {
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()
      val gameActor = system.actorOf(GameActor.props(
        PlayerInfo(player1Probe.ref, "player1"),
        PlayerInfo(player2Probe.ref, "player2")))

      gameActor.tell(MakeMove(0, 0), player1Probe.ref)

      player1Probe.expectMsg(GameStarted("player2"))
      player2Probe.expectMsg(GameStarted("player1"))
      player1Probe.expectMsg(PlayerMadeMove("player1", 0, 0, "X"))
      player2Probe.expectMsg(PlayerMadeMove("player1", 0, 0, "X"))
    }


    "send error when player is invalid" in {
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()
      val player3Probe = TestProbe()
      val gameActor = system.actorOf(GameActor.props(
        PlayerInfo(player1Probe.ref, "player1"),
        PlayerInfo(player2Probe.ref, "player2"))
      )

      gameActor.tell(MakeMove(0, 0), player3Probe.ref)

      player3Probe.expectMsg(ErrorResponse("Invalid player"))
    }


    "send error if move is invalid" in {
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()
      val gameActor = system.actorOf(GameActor.props(
        PlayerInfo(player1Probe.ref, "player1"),
        PlayerInfo(player2Probe.ref, "player2"))
      )

      player2Probe.expectMsg(GameStarted("player1"))

      gameActor.tell(MakeMove(0, 0), player2Probe.ref)

      player2Probe.expectMsg(InvalidMark)
    }

    "send win and loose messages if game is over" in {
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()
      val gameActor = system.actorOf(GameActor.props(
        new TicTacToeGame(Vector(
          Vector(X, O, O),
          Vector(X, Empty, Empty),
          Vector(Empty, Empty, Empty)
        )),
        PlayerInfo(player1Probe.ref, "player1"),
        PlayerInfo(player2Probe.ref, "player2"))
      )

      player1Probe.expectMsg(GameStarted("player2"))
      player2Probe.expectMsg(GameStarted("player1"))

      gameActor.tell(MakeMove(2, 0), player1Probe.ref)

      player1Probe.expectMsg(PlayerMadeMove("player1", 2, 0, "X"))
      player2Probe.expectMsg(PlayerMadeMove("player1", 2, 0, "X"))
      player1Probe.expectMsg(com.example.akkatest.server.Win())
      player2Probe.expectMsg(Loss())
    }

    "send draw messages if game is over" in {
      val player1Probe = TestProbe()
      val player2Probe = TestProbe()
      val gameActor = system.actorOf(GameActor.props(
        new TicTacToeGame(Vector(
          Vector(X, O, X),
          Vector(X, O, O),
          Vector(O, X, Empty)
        )),
        PlayerInfo(player1Probe.ref, "player1"),
        PlayerInfo(player2Probe.ref, "player2"))
      )

      player1Probe.expectMsg(GameStarted("player2"))
      player2Probe.expectMsg(GameStarted("player1"))

      gameActor.tell(MakeMove(2, 2), player1Probe.ref)

      player1Probe.expectMsg(PlayerMadeMove("player1", 2, 2, "X"))
      player2Probe.expectMsg(PlayerMadeMove("player1", 2, 2, "X"))
      player1Probe.expectMsg(com.example.akkatest.server.Draw())
      player2Probe.expectMsg(com.example.akkatest.server.Draw())
    }
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }
}

package com.example.akkatest.session

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit, TestProbe}
import com.example.akkatest.common.StabEchoReceiver
import com.example.akkatest.game.OutOfRange
import com.example.akkatest.matchmaking.MatchMakingStatuses.{AddedToMatch, NotMatched}
import com.example.akkatest.matchmaking.{MatchEnded, MatchPlayers}
import com.example.akkatest.players.PlayerRepository.RegisterMessage
import com.example.akkatest.players.RegisterResults.{Exists, Registered}
import com.example.akkatest.server._
import com.example.akkatest.session.Session.AddToMatching
import com.example.akkatest.session.SessionRepository.LoginMessage
import com.example.akkatest.session.SessionRepository.LoginResults.{Successful, UserNotExists}
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class SessionTest extends TestKit(ActorSystem("testSystem"))
  with ImplicitSender
  with WordSpecLike
  with MustMatchers
  with DefaultTimeout
  with BeforeAndAfterAll {

  val notInitializedMessage = "not initialized state"
  val anonymousMessage = "anonymous state"
  val authorizedMessage = "authorized state"
  val onlineMessage = "online state"
  val matchingMessage = "matching state"

  implicit val executionContext = system.dispatcher

  "session" must {

    //region not initialized state tests

    trait notInitializedScope {
      val socket = TestProbe()
      val gateway = TestProbe()
      val id = UUID.randomUUID()
      val session = system.actorOf(Props(new Session(id, gateway.ref) with StabEchoReceiver {

        override def notInitialized(): Receive = stabReceive(notInitializedMessage) orElse super.notInitialized()

        override def anonymous(socket: ActorRef): Receive = stabReceive(anonymousMessage) orElse super.anonymous(socket)

        override def authorized(socket: ActorRef, username: String): Receive =
          stabReceive(authorizedMessage) orElse super.authorized(socket, username)
      }))
    }

    "starts in not initialized state" in new notInitializedScope {
      session ! notInitializedMessage
      expectMsg(notInitializedMessage)
    }

    "send socket actor change state to anonymous" in new notInitializedScope {
      session ! ('income, socket.ref)
      session ! anonymousMessage
      expectMsg(anonymousMessage)
    }

    //endregion

    //region anonymous state tests

    trait anonymousScope {
      val socket = TestProbe()
      val gateway = TestProbe()
      val id = UUID.randomUUID()
      val session = system.actorOf(Props(new Session(id, gateway.ref) with StabEchoReceiver {
        override def receive: Receive = anonymous(socket.ref)

        override def anonymous(socket: ActorRef): Receive =
          stabReceive(anonymousMessage) orElse super.anonymous(socket)

        override def authorized(socket: ActorRef, username: String): Receive =
          stabReceive(authorizedMessage) orElse super.authorized(socket, username)
      }))
    }

    "successful registration returns ok response" in new anonymousScope {
      session.tell(Register("user1", "pass1"), socket.ref)
      gateway.expectMsg(RegisterMessage("user1", "pass1"))
      gateway.reply(Registered)
      socket.expectMsg(Ok())
    }

    "failed registration returns error response" in new anonymousScope {
      session.tell(Register("user1", "pass1"), socket.ref)
      gateway.expectMsg(RegisterMessage("user1", "pass1"))
      gateway.reply(Exists)
      socket.expectMsg(Error(Exists.toString))
    }

    "successful auth return ok and change state to authorized" in new anonymousScope {
      session.tell(Login("user1", "pass1"), socket.ref)
      gateway.expectMsg(LoginMessage(id, "user1", "pass1"))
      gateway.reply(Successful("user1"))
      socket.expectMsg(Ok())
      session ! authorizedMessage
      expectMsg(authorizedMessage)
    }

    "failed auth return error response and doesn't change state" in new anonymousScope {
      session.tell(Login("user1", "pass1"), socket.ref)
      gateway.expectMsg(LoginMessage(id, "user1", "pass1"))
      gateway.reply(UserNotExists("user1"))
      socket.expectMsg(Error(UserNotExists("user1").toString))
      session ! anonymousMessage
      expectMsg(anonymousMessage)
    }

    //endregion

    //region authorized state tests

    trait authorizedScope {
      val socket = TestProbe()
      val gateway = TestProbe()
      val username = "user1"
      val id = UUID.randomUUID()
      val session = system.actorOf(Props(new Session(id, gateway.ref) with StabEchoReceiver {
        override def authorized(socket: ActorRef, username: String): Receive =
          stabReceive(authorizedMessage) orElse super.authorized(socket, username)

        override def online(socket: ActorRef, username: String): Receive =
          stabReceive(onlineMessage) orElse super.online(socket, username)

        override def receive: Receive = super.authorized(socket.ref, username)
      }))
    }

    "authorized user send AddToMatching request" in new authorizedScope {
      session ! ReadyToMatch()
      gateway.expectMsg(AddToMatching(username, session))
    }

    "authorized user returns error on socket if user is not matched" in new authorizedScope {
      session ! NotMatched
      socket.expectMsgClass(classOf[Error])
    }

    "authorized user returns socket and change status if " in new authorizedScope {
      session ! AddedToMatch
      socket.expectMsg(Ok())
      session ! onlineMessage
      expectMsg(onlineMessage)
    }

    //endregion

    //region online state tests

    trait onlineScope {
      val socket = TestProbe()
      val gateway = TestProbe()
      val username = "user1"
      val id = UUID.randomUUID()
      val gameActor = TestProbe()
      val session = system.actorOf(Props(new Session(id, gateway.ref) with StabEchoReceiver {
        override def matching(socket: ActorRef, username: String, game: ActorRef): Receive =
          stabReceive(matchingMessage) orElse super.matching(socket, username, game)

        override def receive: Receive = super.online(socket.ref, username)
      }))
    }

    "view opponents in online state" in new onlineScope {
      session.tell(GetOpponents(), socket.ref)
      gateway.expectMsg(GetOpponents())
      session.tell(OpponentsList(Vector("user2")), gateway.ref)
      socket.expectMsg(OpponentsList(Vector("user2")))
    }

    "send to gateway matching request" in new onlineScope {
      val opponent = "user2"
      session.tell(MatchWith(opponent), socket.ref)
      gateway.expectMsg(MatchPlayers(username, opponent))
    }

    "returns error on game start failing" in new onlineScope {
      session ! NotMatched
      socket.expectMsg(Error(NotMatched.toString))
    }

    "moves to matching state on game starting" in new onlineScope {
      val gameStartRequest = GameStarted("user2")
      session.tell(gameStartRequest, gameActor.ref)
      socket.expectMsg(gameStartRequest)
      session ! matchingMessage
      expectMsg(matchingMessage)
    }

    //endregion

    //region matching state tests

    trait matchingScope {
      val socket = TestProbe()
      val gateway = TestProbe()
      val username = "user1"
      val id = UUID.randomUUID()
      val gameActor = TestProbe()
      val session = system.actorOf(Props(new Session(id, gateway.ref) with StabEchoReceiver {
        override def authorized(socket: ActorRef, username: String): Receive =
          stabReceive(authorizedMessage) orElse super.authorized(socket, username)

        override def receive: Receive = super.matching(socket.ref, username, gameActor.ref)
      }))
    }

    "forwards player's moves to game actor" in new matchingScope {
      val moveRequest = MakeMove(0, 0)
      session.tell(moveRequest, socket.ref)
      gameActor.expectMsg(moveRequest)
    }

    "forwards game errors to socket" in new matchingScope {
      session.tell(OutOfRange, gameActor.ref)
      socket.expectMsg(Error(OutOfRange.toString))
    }

    "forwards to socket opponent's moves" in new matchingScope {
      val moveResult = PlayerMadeMove("user2", 0, 0, "X")
      session.tell(moveResult, gameActor.ref)
      socket.expectMsg(moveResult)
    }

    "on game's end moves to online state" in new matchingScope {
      session.tell(Win(), gameActor.ref)
      socket.expectMsg(Win())
      gateway.expectMsg(MatchEnded(username))
      session ! authorizedMessage
      expectMsg(authorizedMessage)
    }

    //endregion

  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

}

package com.example.akkatest.session

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit, TestProbe}
import com.example.akkatest.common.StabEchoReceiver
import com.example.akkatest.players.RegisterResults.{Exists, Registered}
import com.example.akkatest.server.{Error, Login, Ok, Register}
import com.example.akkatest.session.ServerGateway.LoginResults.{Successful, UserNotExists}
import com.example.akkatest.session.ServerGateway.{LoginMessage, RegisterMessage}
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

  implicit val executionContext = system.dispatcher

  "session" must {

    trait scope {
      val socket = TestProbe()
      val matchMakingActor = TestProbe()
      val parent = TestProbe()
      val id = UUID.randomUUID()
      val session = parent.childActorOf(Props(new Session(id, matchMakingActor.ref) with StabEchoReceiver {

        override def notInitialized(): Receive = stabReceive(notInitializedMessage) orElse super.notInitialized()

        override def anonymous(socket: ActorRef): Receive = stabReceive(anonymousMessage) orElse super.anonymous(socket)

        override def authorized(socket: ActorRef, username: String): Receive =
          stabReceive(authorizedMessage) orElse super.authorized(socket, username)
      }))
    }

    "starts in not initialized state" in new scope {
      session ! notInitializedMessage
      expectMsg(notInitializedMessage)
    }

    "send socket actor change state to anonymous" in new scope {
      session ! ('income, socket.ref)
      session ! anonymousMessage
      expectMsg(anonymousMessage)
    }

    "successful registration returns ok response" in new scope {
      session ! ('income, socket.ref)
      session.tell(Register("user1", "pass1"), socket.ref)
      parent.expectMsg(RegisterMessage("user1", "pass1"))
      parent.reply(Registered)
      socket.expectMsg(Ok())
    }

    "failed registration returns error response" in new scope {
      session ! ('income, socket.ref)
      session.tell(Register("user1", "pass1"), socket.ref)
      parent.expectMsg(RegisterMessage("user1", "pass1"))
      parent.reply(Exists)
      socket.expectMsg(Error(Exists.toString))
    }

    "successful auth return ok and change state to authorized" in new scope {
      session ! ('income, socket.ref)
      session.tell(Login("user1", "pass1"), socket.ref)
      parent.expectMsg(LoginMessage(id, "user1", "pass1"))
      parent.reply(Successful("user1"))
      socket.expectMsg(Ok())
      session ! authorizedMessage
      expectMsg(authorizedMessage)
    }

    "failed auth return error response and doesn't change state" in new scope {
      session ! ('income, socket.ref)
      session.tell(Login("user1", "pass1"), socket.ref)
      parent.expectMsg(LoginMessage(id, "user1", "pass1"))
      parent.reply(UserNotExists("user1"))
      socket.expectMsg(Error(UserNotExists("user1").toString))
      session ! anonymousMessage
      expectMsg(anonymousMessage)
    }
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

}

package com.example.akkatest.session

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit, TestProbe}
import com.example.akkatest.common.StabEchoReceiver
import com.example.akkatest.players.RegisterResults.{Exists, Registered}
import com.example.akkatest.server.{ErrorResponse, LoginRequest, Ok, RegisterRequest}
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

  val anonymousMessage = "anonymous state"
  val authorizedMessage = "authorized state"

  implicit val executionContext = system.dispatcher

  "session" must {

    trait scope {
      val matchMakingActor = TestProbe()
      val parent = TestProbe()
      val id = UUID.randomUUID()
      val session = parent.childActorOf(Props(new Session(id, matchMakingActor.ref) with StabEchoReceiver {
        override def anonymous(): Receive = stabReceive(anonymousMessage) orElse  super.anonymous()

        override def authorized(username: String): Receive =
          stabReceive(authorizedMessage) orElse super.authorized(username)
      }))
    }

    "starts in anonymous state" in new scope {
      session ! anonymousMessage
      expectMsg(anonymousMessage)
    }

    "successful registration returns ok response" in new scope {

      session ! RegisterRequest("user1", "pass1")
      parent.expectMsg(RegisterMessage("user1", "pass1"))
      parent.reply(Registered)
      expectMsg(Ok())
    }

    "failed registration returns error response" in new scope {

      session ! RegisterRequest("user1", "pass1")
      parent.expectMsg(RegisterMessage("user1", "pass1"))
      parent.reply(Exists)
      expectMsg(ErrorResponse(Exists.toString))
    }

    "successful auth return ok and change state to authorized" in new scope {

      session ! LoginRequest("user1", "pass1")
      parent.expectMsg(LoginMessage(id, "user1", "pass1"))
      parent.reply(Successful)
      expectMsg(Ok())
      session ! authorizedMessage
      expectMsg(authorizedMessage)
    }

    "failed auth return error response and doesn't change state" in new scope {

      session ! LoginRequest("user1", "pass1")
      parent.expectMsg(LoginMessage(id, "user1", "pass1"))
      parent.reply(UserNotExists)
      expectMsg(ErrorResponse(UserNotExists.toString))
      session ! anonymousMessage
      expectMsg(anonymousMessage)
    }
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

}

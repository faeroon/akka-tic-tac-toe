package com.example.akkatest.server

import java.util.concurrent.TimeUnit

import com.example.akkatest.matchmaking.MatchMakingActor
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.Timeout
import com.example.akkatest.game.GameManagerActor
import com.example.akkatest.players.PlayerRepository
import com.example.akkatest.session._

import scala.concurrent.duration.FiniteDuration
import scala.io.StdIn

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
object WebServer {

  implicit val system = ActorSystem("tic-tac-toe-server")
  implicit val materializer = ActorMaterializer()

  implicit val executionContext = system.dispatcher

  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  def initWebSocketActor(session: ActorRef): Flow[Message, Message, Any] = {
    val client = system.actorOf(Props(new WebSocketActor(session)))
    val in = Sink.actorRef(client, 'sinkclose)
    val out = Source.actorRef(8, OverflowStrategy.fail).mapMaterializedValue { a =>
      client ! ('income -> a)
      a
    }.keepAlive(FiniteDuration(5, TimeUnit.SECONDS), () => TextMessage.Strict("HEART_BEAT"))
    Flow.fromSinkAndSource(in, out)
  }

  def main(args: Array[String]): Unit = {

    val playerRepository = system.actorOf(Props[PlayerRepository], name = "player-repository")
    val gameManagerActor = system.actorOf(Props[GameManagerActor], name = "game-manager")
    val matchMakingActor = system.actorOf(Props(new MatchMakingActor(gameManagerActor)), name = "matchmaking")
    val serverGateway = system.actorOf(Props[ServerGateway](new ServerGateway(playerRepository, matchMakingActor)),
      name = "server-gateway")

    val route = {
      path("ws") {
        get {
          onSuccess(serverGateway.ask(StartSession()).mapTo[ActorRef]) {
            session => handleWebSocketMessages(initWebSocketActor(session))
          }
        }
      }
    }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println("RETURN to stop...")
    StdIn.readLine()

    bindingFuture.flatMap(_.unbind())
    .onComplete(_ => system.terminate())
  }
}

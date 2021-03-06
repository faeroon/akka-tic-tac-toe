package com.example.akkatest.server

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.util.Timeout
import com.example.akkatest.session.SessionRepository.StartSession
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration.FiniteDuration
import scala.io.StdIn

/**
  * Akka web-socket game server
  *
  * @author Denis Pakhomov.
  * @version 1.0
  */
object WebServer {

  val configuration = ConfigFactory.load()
  implicit val system = ActorSystem("tic-tac-toe-server", configuration)
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  /**
    * create adapter actor for web-socket connection
    *
    * @param session player session actor
    * @return
    */
  def initWebSocketActor(session: ActorRef): Flow[Message, Message, Any] = {
    val client = system.actorOf(WebSocketAdapter.props(session))
    val in = Sink.actorRef(client, 'sinkclose)
    val out = Source.actorRef(8, OverflowStrategy.fail).mapMaterializedValue { a =>
      client ! ('income -> a)
      a
    }.keepAlive(FiniteDuration(5, TimeUnit.SECONDS), () => TextMessage.Strict("""{"$type": "HeartBeat"}"""))
    Flow.fromSinkAndSource(in, out)
  }

  def main(args: Array[String]): Unit = {

    val serverGateway = system.actorOf(ServerGateway.props(), name = "gateway")

    // create /ws endpoint for web socket connections
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

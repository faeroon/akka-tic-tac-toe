package com.example.akkatest.server

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.routing.FromConfig
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.example.akkatest.players.PlayerRepository
import com.typesafe.config.ConfigFactory

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
object PlayersRepoServer {
  val configuration = ConfigFactory.load()
  implicit val system = ActorSystem("players-repo", configuration.getConfig("players-repo"))
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  def start(): Unit = {
    val playersRepoActor = system.actorOf(FromConfig.props(PlayerRepository.props()), "players")
  }
}

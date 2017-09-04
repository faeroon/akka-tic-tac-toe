package com.example.akkatest

import com.example.akkatest.server.{PlayersRepoServer, WebServer}

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
object AkkaApp {
  def main(args: Array[String]): Unit = {
    if (args.length == 2 && (args(0) == "--role" || args(0) == "-r")) {
      args(1) match {
        case "web-server" => WebServer.start()
        case "players-repo" => PlayersRepoServer.start()
        case _ => println("invalid server role")
      }
    } else println("invalid arguments")
  }
}

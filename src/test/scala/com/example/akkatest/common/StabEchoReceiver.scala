package com.example.akkatest.common

import akka.actor.Actor

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
trait StabEchoReceiver { self: Actor =>
  def stabReceive(state: String): Receive = {
    case msg: String if msg == state => sender ! state
  }

}

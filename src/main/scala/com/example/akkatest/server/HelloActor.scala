package com.example.akkatest.server

import akka.actor.Actor

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
class HelloActor extends Actor{
  override def receive = {
    case "hello" => sender() ! "hello, dude!"
    case _ => sender() ! "I can't understand you..."
  }
}

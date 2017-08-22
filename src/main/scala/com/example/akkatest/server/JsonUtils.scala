package com.example.akkatest.server

import akka.http.scaladsl.model.ws.TextMessage
import io.circe.generic.extras.{Configuration, semiauto => fancy}
import io.circe.syntax._
import io.circe.{Decoder, Encoder}

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
trait JsonUtils {

  implicit val customConfig: Configuration = Configuration.default.withDefaults.withSnakeCaseKeys
    .withDiscriminator("type")

  implicit val gameResponseEncoder: Encoder[GameResponse] = fancy.deriveEncoder[GameResponse]
  implicit val gameRequestDecoder: Decoder[GameRequest] = fancy.deriveDecoder[GameRequest]

  implicit class GameResponseConverter(gameResponse: GameResponse) {
    def toTextMessage: TextMessage = TextMessage.Strict(gameResponse.asJson.toString())
  }
}
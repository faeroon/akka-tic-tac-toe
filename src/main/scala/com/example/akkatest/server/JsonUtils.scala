package com.example.akkatest.server

import akka.http.scaladsl.model.ws.TextMessage
import io.circe.Decoder.instance
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}

/**
  * @author Denis Pakhomov.
  * @version 1.0
  */
trait JsonUtils {

  implicit val gameResponseEncoder: Encoder[GameResponse] = deriveEncoder[GameResponse]

  implicit val gameRequestDecoder: Decoder[GameRequest] = instance { c =>
    c.downField("type").focus match {
      case Some(t) => t.asString match {

        case Some(string) if string == "READY_TO_MATCH" => c.get[ReadyToMatchRequest]("body")
        case Some(string) if string == "GET_OPPONENTS" => c.get[GetOpponentsRequest]("body")
        case Some(string) if string == "MATCH_WITH" => c.get[MatchWithRequest]("body")
        case Some(string) if string == "MAKE_MOVE" => c.get[MakeMove]("body")
        case Some(string) if string == "REGISTER" => c.get[RegisterRequest]("body")
        case Some(string) if string == "LOGIN" => c.get[LoginRequest]("body")

        case Some(string) => Left(DecodingFailure(s"unknown type $string", c.history))
        case None => Left(DecodingFailure("json field \"type\" not found", c.history))
      }
      case None => Left(DecodingFailure("json field \"type\" has invalid type", c.history))
    }
  }

  def camelToUnderscores(name: String) = "[A-Z\\d]".r.replaceAllIn(name, {m =>
    "_" + m.group(0).toLowerCase()
  })

  case class GameResponseWrapper(requestType: String, body: GameResponse)

  implicit class GameResponseConverter(gameResponse: GameResponse) {
    def toTextMessage: TextMessage =
      TextMessage.Strict(GameResponseWrapper(camelToUnderscores(gameResponse.getClass.getSimpleName).toUpperCase(),
        gameResponse).asJson.toString())
  }
}
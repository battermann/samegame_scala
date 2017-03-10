package com.example

import java.util.UUID

import cats.implicits._
import com.example.samegame._
import org.scalatest.FunSuite
import com.example.serialization.implicits._
import play.api.libs.json.{JsSuccess, Json}

class SerializationTests extends FunSuite {

  test("Given an event, when serialized and deserialized, the result should be equal to original event") {
    val width = 3
    val height = 3
    val columns = (1 to width).map(_ => Column((1 to height).map(_ => Filled(Color(4))).toList))
    val gameId = GameId(UUID.randomUUID())

    val result = for {
      board <- Board.create(columns.toList)
    } yield {
      val event = GameStarted(gameId, board)
      val jsonEvent = Json.toJson(event)
      jsonEvent.validate[Event] match {
        case JsSuccess(e, _) => assert(e == event)
        case err => fail(s"$err")
      }
    }

    result.fold(err => fail(s"$err"), _ => ())
  }
}
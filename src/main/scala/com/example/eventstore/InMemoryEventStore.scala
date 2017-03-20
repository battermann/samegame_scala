package com.example.eventstore

import cats.implicits._
import com.example.samegame.{ConcurrencyFailure, DomainMessage, Event}
import com.example.serialization.implicits._
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

object InMemoryEventStore {
  type Data = String
  private var streams = Map[String, List[(Data, Int)]]()

  private def getVersion(stream: List[(Data, Int)]): Int = stream.map(_._2).max

  def appendToStream(streamId: String, expectedVersion: Int, events: List[Event]): Future[Either[DomainMessage, Unit]] = {
    val streamOrError = streams.get(streamId) match {
      case Some(stream) if getVersion(stream) == expectedVersion =>
        Right(stream)
      case None if expectedVersion == -1 =>
        val stream = List[(Data, Int)]()
        streams = streams + (streamId -> stream)
        Right(stream)
      case _ =>
        Left(ConcurrencyFailure(""): DomainMessage)
    }

    val eventsWithVersion = events
      .zipWithIndex
      .map { case (e, i) => (Json.stringify(Json.toJson(e)), expectedVersion + i + 1)}

    val result = streamOrError
      .map(s => {
          streams = streams + (streamId -> (s ++ eventsWithVersion))
      })
    result.pure[Future]
  }

  def readFromStream(streamId: String)(implicit ec: ExecutionContext): Future[Either[DomainMessage, List[Event]]] = {
    val events = streams.get(streamId) match {
      case Some(stream) =>
        stream
          .sortBy(_._2)
          .map(e => Json.parse(e._1).as[Event])
      case None => List()
    }
    Right(events).pure[Future]
  }
}

package com.example

import cats.data._
import cats.implicits._
import com.example.eventstore.EventStore
import com.example.eventstore.FutureEither.FutureEither
import com.example.samegame._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object CommandHandling {

  private def toStreamId(gameId: GameId) = s"samegame:${gameId.id.toString}"

  def handle(store: EventStore[Event], publisher: Option[(Event => Unit)] = None)
            (gameId: GameId, command: Command): FutureEither[List[Event]] =
    for {
      events <- store.readFromStream(toStreamId(gameId))
      StateVersionPair(game, Version(version)) = Domain.replay(events)
      newEvents <- EitherT(Future.successful(Domain.decide(gameId, game, command)))
      _ <- store.appendToStream(toStreamId(gameId), version, newEvents)
      _ = publisher match {
        case Some(p) => newEvents.foreach(p)
        case _ => ()
      }
    } yield newEvents
}

package com.example

import cats.implicits._
import com.example.utils.Composition.FutureEither
import com.example.eventstore.EventStore
import com.example.samegame._
import com.example.utils.?

import scala.concurrent.ExecutionContext.Implicits.global

object CommandHandling {

  private def toStreamId(gameId: GameId) = s"samegame:${gameId.id.toString}"

  def handle(store: EventStore[Event])(gameId: GameId, command: Command): FutureEither[List[Event]] =
    for {
      events    <- ? <~ store.readFromStream(toStreamId(gameId))
      StateVersionPair(game, Version(version)) = SameGame.replay(events)
      newEvents <- ? <~ SameGame.decide(gameId, game, command)
      _         <- ? <~ store.appendToStream(toStreamId(gameId), version, newEvents)
    } yield newEvents
}

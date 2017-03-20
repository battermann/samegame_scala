package com.example

import java.util.UUID

import com.example.eventstore.{EventStore, InMemoryEventStore}
import com.example.samegame.{Command, DomainMessage, Event, GameId}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object BddSpecification {

  implicit val akkaSystem = akka.actor.ActorSystem()

  def given(id: String, events: List[Event]): (String, List[Event]) = (id, events)

  implicit class When(idEventsPair: (String, List[Event])) {
    def when(command: Command): (String, List[Event], Command) = {
      val (id, events) = idEventsPair
      (id, events, command)
    }
  }

  implicit class Expect(givenWhen: (String, List[Event], Command)) {
    def expect(expected: Either[DomainMessage, List[Event]]): Unit = {
      val (id, events, command) = givenWhen
      val store = EventStore(InMemoryEventStore.appendToStream, InMemoryEventStore.readFromStream)
      if(events.nonEmpty) {
        Await.result(store.appendToStream(s"samegame:$id", -1, events), Duration.Inf)
      }

      val handle = CommandHandling.handle(store) _
      val errorOrEvents = Await.result(handle(GameId(UUID.fromString(id)), command).value, Duration.Inf)
      val pass = errorOrEvents == expected
      val msg =
        s"""
           |    Given:
           |    ${events.map(_.toString).mkString("\n")}
           |
           |    When:
           |    $command
           |
           |    Expect:
           |    ${expected.fold(_.toString, _.map(_.toString).mkString("\n"))}
           |
           |    But got:
           |    ${errorOrEvents.fold(_.toString, _.map(_.toString).mkString("\n"))}
           |
         """.stripMargin

      assert(pass, msg)
    }
  }
}

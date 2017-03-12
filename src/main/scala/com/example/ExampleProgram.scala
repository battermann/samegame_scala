package com.example

import java.util.UUID

import cats.data._
import cats.implicits._
import com.example.eventstore._
import com.example.samegame._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ExampleProgram {
  def main(args: Array[String]): Unit = {
    implicit val akkaSystem = akka.actor.ActorSystem()

    //   0 1 2
    // 2 g b r
    // 1 r r r
    // 0 g r b
    // can be solved like this: (0,1), (1,0), (0,1); (<column index>, <row index>)
    // final score 1009

    //val store = EventStore(InMemoryEventStore.appendToStream, InMemoryEventStore.readFromStream)
    val store = EventStore(RedisEventStore.appendToStream("localhost", 6379, "samegame:commits"), RedisEventStore.readFromStream("localhost", 6379, "samegame:commits"))
    val handle = CommandHandling.handle(store, Some(e => println(e))) _

    val id = GameId(UUID.randomUUID())
    val columns = List(
      Column(List(Filled(Gray), Filled(Red), Filled(Gray))),
      Column(List(Filled(Red), Filled(Red), Filled(Blue))),
      Column(List(Filled(Blue), Filled(Red), Filled(Red)))
    )

    val result = for {
      board <- EitherT(Future.successful(Board.create(columns)))
      _     <- handle(id, StartNewGame(board))
      _     <- handle(id, RemoveGroup(Position(0, 1)))
      _     <- handle(id, RemoveGroup(Position(1, 0)))
      _     <- handle(id, RemoveGroup(Position(0, 1)))
    } yield()

    Await.result(result.value, Duration.Inf)
      .fold(err => println(s"[ERROR] $err"), _ => ())

    akkaSystem.terminate()
  }
}

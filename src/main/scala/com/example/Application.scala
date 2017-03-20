package com.example

import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import java.util.UUID

import akka.actor.ActorSystem
import cats.data._
import cats.implicits._
import com.example.eventstore._
import com.example.samegame._
import com.example.utils.Composition.FutureEither
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Random, Try}

object Application {

  sealed trait CliCmd
  final case object NewLine extends CliCmd
  final case class StartGame(width: Int, height: Int) extends CliCmd
  final case class Play(gameId: String, column: Int, row: Int) extends CliCmd
  final case object Exit extends CliCmd

  private val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard

  private def parseInput(input: String): Option[CliCmd] = {
    input.split("[ ]+").map(_.trim).toList match {
      case "exit" :: Nil | ":q" :: Nil => Some(Exit)
      case "start" :: w :: h :: Nil =>
        for {
          width <- Try(w.toInt).toOption
          height <- Try(h.toInt).toOption
        } yield StartGame(width, height)
      case "play" :: id :: c :: r :: Nil =>
        for {
          column <- Try(c.toInt).toOption
          row <- Try(r.toInt).toOption
        } yield Play(id, column, row)
      case "" :: Nil => Some(NewLine)
      case _ => None
    }
  }

  private def promptAndGetInput() = {
    print("> ")
    parseInput(scala.io.StdIn.readLine())
  }

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load()
      .withValue("akka.loglevel", ConfigValueFactory.fromAnyRef("OFF"))
      .withValue("akka.stdout-loglevel", ConfigValueFactory.fromAnyRef("OFF"))

    implicit val akkaSystem = akka.actor.ActorSystem("same-game-akka-system", config)

    val rnd = Random

    val handle = withInMemoryStore()
    //val handle = withRedisStore()

    @tailrec
    def loop(cmd: Option[CliCmd]): Unit = {
      cmd match {

        case Some(Exit) => ()

        case Some(NewLine) =>
          val cmd = promptAndGetInput()
          loop(cmd)

        case Some(StartGame(width, height)) =>
          val id = GameId(UUID.randomUUID())
          val columns = (1 to width)
            .map(_ => Column((1 to height)
              .map(_ => Filled(Color(rnd.nextInt(5))))
              .toList))
            .toList

          val result =
            EitherT.fromEither[Future](Board.create(columns))
              .map(StartNewGame)
              .flatMap(cmd => handle(id, cmd))

          Await.result(result.value, Duration.Inf)
            .fold(
              err => println(s"[ERROR] $err"),
              _ => {
                val selection = new StringSelection(id.id.toString)
                clipboard.setContents(selection, selection)
                println(s"game-id ${id.id.toString} (copied to clipboard)")
              })

          val cmd = promptAndGetInput()
          loop(cmd)

        case Some(Play(id, column, row)) =>
          val maybeGameId = Try(UUID.fromString(id))
            .toOption
            .toRight(InvalidGameIdFormat)
            .map(GameId)

          val result = EitherT.fromEither[Future](maybeGameId)
            .map(gameId => handle(gameId, RemoveGroup(Position(column, row))))

          Await.result(result.value, Duration.Inf)
            .fold(
              err => println(s"[ERROR] $err"),
              _ => ())

          val cmd = promptAndGetInput()
          loop(cmd)

        case None =>
          println("unknown input")
          val cmd = promptAndGetInput()
          loop(cmd)
      }
    }

    println(
      """
        |Welcome to the SameGame CLI
        |
        |To start a new game type 'start <width> <height>'
        |
        |To make a move type 'play <game-id> <column> <row>'
        |    - columns starting from the left with 0
        |    - rows starting from the bottom with 0
        |
        |Quit with 'exit'
      """.stripMargin)
    loop(Some(NewLine))
    akkaSystem.terminate()
  }

  def withRedisStore()(implicit as: ActorSystem): (GameId, Command) => FutureEither[List[Event]] = {
    val store = EventStore(RedisEventStore.appendToStream("localhost", 6379, "samegame:commits"), RedisEventStore.readFromStream("localhost", 6379, "samegame:commits"))
    CommandHandling.handle(store)
  }

  def withInMemoryStore(): (GameId, Command) => EitherT[Future, DomainMessage, Unit] = {
    val store = EventStore(InMemoryEventStore.appendToStream, InMemoryEventStore.readFromStream)
    (id: GameId, cmd: Command) => for {
      events <- CommandHandling.handle(store)(id, cmd)
    } yield {
      events.foreach(println)
    }
  }
}

package com.example

import java.util.UUID

import cats.data._
import cats.implicits._
import com.example.eventstore._
import com.example.samegame._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Application {

  sealed trait CliCmd
  case object NewLine extends CliCmd
  case class StartGame(width: Int, height: Int) extends CliCmd
  case class RemoveGroup(gameId: String, column: Int, row: Int) extends CliCmd
  case object Exit extends CliCmd
  case object Unknown extends CliCmd

  private def parseInput(input: String): CliCmd = {
    input.split(" ").map(_.trim).toList match {
      case "exit" :: Nil => Exit
      case ":q" :: Nil => Exit
      case _ => Unknown
    }
  }

  private def promptAndGetInput() = {
    print("> ")
    parseInput(scala.io.StdIn.readLine())
  }

  @tailrec
  private def loop(cmd: CliCmd): Unit = {
    cmd match {
      case Exit => ()
      case NewLine =>
        val cmd = promptAndGetInput()
        loop(cmd)
      case _ =>
        loop(NewLine)
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val akkaSystem = akka.actor.ActorSystem()
    loop(NewLine)
    akkaSystem.terminate()
  }
}

package com.example

import java.util.UUID

import cats._
import cats.data._
import cats.implicits._
import com.example.eventstore._
import com.example.samegame._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

object Application {

  sealed trait CliCmd
  case object NewLine extends CliCmd
  case class StartGame(width: Int, height: Int) extends CliCmd
  case class Play(gameId: String, column: Int, row: Int) extends CliCmd
  case object Exit extends CliCmd

  private def parseInput(input: String): Option[CliCmd] = {
    input.split(" ").map(_.trim).toList match {
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
    implicit val akkaSystem = akka.actor.ActorSystem()

    @tailrec
    def loop(cmd: Option[CliCmd]): Unit = {
      cmd match {
        case Some(Exit) => ()
        case Some(NewLine) =>
          val cmd = promptAndGetInput()
          loop(cmd)
        case _ =>
          println("[error] unknown input")
          val cmd = promptAndGetInput()
          loop(cmd)
      }
    }

    println("Welcome to the SameGame CLI")

    loop(Some(NewLine))
    akkaSystem.terminate()
  }
}

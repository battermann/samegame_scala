package com.example

import java.util.UUID

import cats.implicits._
import com.example.BddSpecification._
import com.example.samegame.{GroupRemoved, _}
import org.scalatest.FunSuite

class SameGameTests extends FunSuite {

  test("""Given GameStarted, board one color only, when RemoveGroup, expect FinalGroupRemoved with score 1049""") {
    val width = 3
    val height = 3
    val columns = (1 to width).map(_ => Column((1 to height).map(_ => Filled(Color(4))).toList))
    val gameId = GameId(UUID.randomUUID())

    val result = for {
      board <- Board.create(columns.toList)
      emptyBoard <- Board.create((1 to width).map(_ => Column.empty(height)).toList)
      cmd = RemoveGroup(Position(0,0))
    } yield {

      given(gameId.id.toString, List(GameStarted(gameId, board)))
        .when(cmd)
        .expect(Right(List(GroupRemoved(gameId, emptyBoard, 1049), GameFinished(gameId))))
    }

    result.fold(err => fail(s"$err"), _ => ())
  }

  test("""Given GameStarted and GroupRemoved twice, when RemoveGroup, expect FinalGroupRemoved with correct board and score 1001""") {
    val height = 3

    val columns1 = List(
      Column(List(Filled(Blue), Filled(Blue), Filled(Blue))),
      Column(List(Filled(Red), Filled(Red), Filled(Red))),
      Column(List(Filled(Green), Filled(Green), Filled(Green)))
    )
    val columns2 = List(
      Column(List(Filled(Red), Filled(Red), Filled(Red))),
      Column(List(Filled(Green), Filled(Green), Filled(Green))),
      Column.empty(height)
    )
    val columns3 = List(
      Column(List(Filled(Green), Filled(Green), Filled(Green))),
      Column.empty(height),
      Column.empty(height)
    )

    val columns4 = List(
      Column.empty(height),
      Column.empty(height),
      Column.empty(height)
    )

    val gameId = GameId(UUID.randomUUID())

    val result = for {
      board1 <- Board.create(columns1)
      board2 <- Board.create(columns2)
      board3 <- Board.create(columns3)
      empty  <- Board.create(columns4)
      cmd = RemoveGroup(Position(0, 0))
    } yield {

      given(gameId.id.toString, List(
        GameStarted(gameId, board1),
        GroupRemoved(gameId, board2, 1),
        GroupRemoved(gameId, board3, 1001)
      ))
        .when(cmd)
        .expect(Right(List(GroupRemoved(gameId, empty, 1001), GameFinished(gameId))))
    }

    result.fold(err => fail(s"$err"), _ => ())
  }

  test("""Given GameStarted, when RemoveGroup, expect FinalGroupRemoved with score -4""") {
    val columns = List(
      Column(List(Filled(Blue), Filled(Gray))),
      Column(List(Filled(Blue), Filled(Green))),
      Column(List(Filled(Gray), Filled(Red)))
    )
    val expected = List(
      Column(List(Filled(Gray), Empty)),
      Column(List(Filled(Green), Empty)),
      Column(List(Filled(Gray), Filled(Red)))
    )
    val gameId = GameId(UUID.randomUUID())

    val result = for {
      board <- Board.create(columns)
      expectedBoard <- Board.create(expected)
      cmd = RemoveGroup(Position(0,0))
    } yield {

      given(gameId.id.toString, List(GameStarted(gameId, board)))
        .when(cmd)
        .expect(Right(List(GroupRemoved(gameId, expectedBoard, -4), GameFinished(gameId))))
    }

    result.fold(err => fail(s"$err"), _ => ())
  }
}
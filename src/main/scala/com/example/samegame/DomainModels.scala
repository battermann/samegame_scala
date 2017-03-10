package com.example.samegame

import java.util.UUID

case class Position(col: Int, row: Int)

sealed trait Color
case object Green extends Color
case object Blue extends Color
case object Red extends Color
case object Brown extends Color
case object Gray extends Color

sealed trait CellState
case class Filled(color: Color) extends CellState
case object Empty extends CellState

case class Cell(position: Position, state: CellState)
case class Group(color: Color, positions: Set[Position])
case class Column(cells: List[CellState]) extends AnyVal
case class Board private (columns: List[Column]) extends AnyVal

sealed trait Game
case object Uninitialized extends Game
case class InProgress(board: Board, score: Int) extends Game
case class Finished(board: Board, score: Int) extends Game

case class Version(version: Int) extends AnyVal
case class StateVersionPair(state: Game, version: Version)
case class GameId(id: UUID) extends AnyVal

object Color {
  def apply(n: Int): Color = {
    n % 5 match {
      case 0 => Green
      case 1 => Blue
      case 2 => Red
      case 3 => Brown
      case 4 => Gray
    }
  }
}

object Position {
  def left(pos: Position): Position = {
    Position(pos.col - 1, pos.row)
  }

  def right(pos: Position): Position = {
    Position(pos.col + 1, pos.row)
  }

  def up(pos: Position): Position = {
    Position(pos.col, pos.row + 1)
  }

  def down(pos: Position): Position = {
    Position(pos.col, pos.row - 1)
  }
}

object Column {
  implicit class CellMapper(column: Column) {
    def map(f: (CellState, Int) => CellState): Column = {
      Column(column.cells.zipWithIndex.map { case (cs, i) => f(cs, i) })
    }

    def shiftDown: Column = {
      val nonEmptyCells = column
        .cells
        .filter(!CellState.isEmpty(_))

      val diff = column.cells.length - nonEmptyCells.length

      Column(nonEmptyCells ++ List.fill(diff)(Empty))
    }
  }

  def empty(height: Int): Column = Column((1 to height).map(_ => Empty).toList)
}

object CellState {
  def isEmpty(cellState: CellState): Boolean = {
    cellState match {
      case Empty => true
      case _     => false
    }
  }
}

object Board {
  def create(columns: List[Column]): Either[DomainMessage, Board] = {
    import cats.implicits._

    Right(columns.length)
      .ensure(BoardWithZeroColumnsNotValid)(_ > 0)
      .map(_ => columns.head.cells.length)
      .ensure(ColumnWithZeroCellsNotValid)(_ > 0)
      .ensure(BoardWithMultipleColumnHeightsNotValid)(height => columns.forall(_.cells.length == height))
      .map(_ => Board(columns))
  }

  implicit class ColumnMapper(board: Board) {
    def map(f: (Column, Int) => Column): Board = {
      Board(board.columns.zipWithIndex.map { case (c, i) => f(c, i) })
    }

    def shiftLeft: Board = {
      val nonEmptyColumns = board
        .columns
        .filter(column => !CellState.isEmpty(column.cells.head))

      val diff = board.columns.length - nonEmptyColumns.length
      val height = board.columns.head.cells.length
      Board(nonEmptyColumns ++ List.fill(diff)(Column.empty(height)))
    }
  }
}
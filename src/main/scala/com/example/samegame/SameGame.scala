package com.example.samegame

import Column.CellMapper
import Board.ColumnMapper

object SameGame {
  private val bonus = 1000

  private def sqr(x: Int) = x * x

  private def calcScore(group: Group) = sqr(group.positions.size - 2)

  private def penalty(numberOfFilledCells: Int) = -sqr(numberOfFilledCells - 2)

  private def getCellState(board: Board, position: Position) = {
    val width = board.columns.length
    val height = board.columns.head.cells.length
    if (position.col >= 0 && position.col < width && position.row >= 0 && position.row < height) {
      board.columns(position.col).cells(position.row)
    } else {
      Empty
    }
  }

  private def findAdjacentWithSameColor(board: Board, position: Position): Set[Position] = {
    getCellState(board, position) match {

      case Filled(color) =>
        Set(
          Position.up(position),
          Position.right(position),
          Position.down(position),
          Position.left(position)
        )
          .map(p => (getCellState(board, p), p))
          .filter {
            case (Filled(c), _) => c == color
            case _ => false
          }
          .map(_._2)

      case Empty => Set()
    }
  }

  private def hasValidMoves(board: Board): Boolean = {
    board
      .columns
      .zipWithIndex
      .exists {
        case (column, colIndex) =>
          column
            .cells
            .zipWithIndex
            .exists {
              case (row, rowIndex) =>
                findAdjacentWithSameColor(board, Position(colIndex, rowIndex)).nonEmpty
            }
      }
  }

  private def filledCells(board: Board): Int = {
    board
      .columns
      .foldLeft(0)((total, column) =>
        column.cells.foldLeft(total)((count, cell) =>
          cell match {
            case Filled(_) => count + 1
            case Empty => count
          })
      )
  }

  private def findGroup(board: Board, position: Position): Option[Group] = {
    def find(toSearch: Set[Position], group: Set[Position]): Set[Position] = {
      if (toSearch.isEmpty) {
        group
      } else {
        val head = toSearch.head
        val cellsWithSameColor = findAdjacentWithSameColor(board, head)
        val cellsFoundSoFar = group + head
        val stillToSearch = (cellsWithSameColor ++ toSearch.tail) -- cellsFoundSoFar
        find(stillToSearch, cellsFoundSoFar)
      }
    }

    getCellState(board, position) match {
      case Filled(color) =>
        val positions = find(Set(position), Set.empty)
        if (positions.size > 1) {
          Some(Group(color, positions))
        } else {
          None
        }
      case _ => None
    }
  }

  private def removeGroup(board: Board, group: Group): Board = {
    board
      .map {
        case (column, colIndex) =>
          column
            .map {
              case (cell, rowIndex) =>
                if (group.positions.contains(Position(colIndex, rowIndex))) {
                  Empty
                } else {
                  cell
                }
            }
            .shiftDown
      }
      .shiftLeft
  }

  private def play(board: Board, position: Position): Option[(Board, Int)] = {
    findGroup(board, position)
      .map(g => (removeGroup(board, g), calcScore(g)))
  }

  private def evaluateGameState(board: Board): (Game, Int) = {
    def isEmpty(board: Board) = filledCells(board) == 0
    if (hasValidMoves(board)) {
      (InProgress(board), 0)
    } else if (isEmpty(board)) {
      (Finished(board), bonus)
    } else {
      (Finished(board), penalty(filledCells(board)))
    }
  }

  // Integration

  def replay(events: List[Event]): StateVersionPair = {

    def apply(game: Game, event: Event): Game = {
      (game, event) match {

        case (Uninitialized, GameStarted(board)) =>
          InProgress(board)

        case (InProgress(_), GroupRemoved(_, board, _)) =>
          InProgress(board)

        case (InProgress(board), GameFinished) =>
          Finished(board)

        case _ => game
      }
    }

    def folder(state: StateVersionPair, event: Event): StateVersionPair = {
      val StateVersionPair(game, Version(v)) = state
      val updatedGame = apply(game, event)
      StateVersionPair(updatedGame, Version(v + 1))
    }

    events.foldLeft(StateVersionPair(Uninitialized, Version(-1)))(folder)
  }

  private def startGame(id: GameId, board: Board): List[Event] = {
    val maybeFinished = if(!hasValidMoves(board)) List(GameFinished) else Nil
    GameStarted(board) :: maybeFinished
  }

  private def tryRemoveGroup(id: GameId, board: Board, position: Position): List[Event] = {
    play(board, position)
      .map {
        case (updatedBoard, score) =>
          evaluateGameState(updatedBoard) match {

            case (InProgress(b), _) =>
              List(GroupRemoved(position, b, score))

            case (Finished(b), bonusOrPenalty) =>
              List(GroupRemoved(position, b, score + bonusOrPenalty), GameFinished)

            case (Uninitialized, _) =>
              List() // impossible case
          }
      }
      .getOrElse(List())
  }

  def decide(id: GameId, game: Game, command: Command): Either[DomainMessage, List[Event]] = {
    (game, command) match {

      case (Uninitialized, StartNewGame(board)) =>
        Right(startGame(id, board))

      case (InProgress(board), RemoveGroup(position)) =>
        Right(tryRemoveGroup(id, board, position))

      case (state, cmd) => Left(InvalidOperation(state, cmd))
    }
  }
}

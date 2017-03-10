package com.example.samegame

import Column.CellMapper
import Board.ColumnMapper

object Domain {
  private val bonus = 1000

  private def sqr(x: Int) = x * x

  private def calcScore(group: Group) = sqr(group.positions.size - 2)

  private def penalty(numberOfStonesLeft: Int) = -sqr(numberOfStonesLeft - 2)

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
            case Filled(_) => total + 1
            case Empty => total
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

  private def evaluateGameState(board: Board, score: Int): Game = {
    def isEmpty(board: Board) = filledCells(board) == 0
    if (hasValidMoves(board)) {
      InProgress(board, score)
    } else if (isEmpty(board)) {
      Finished(board, score + bonus)
    } else {
      Finished(board, score + penalty(filledCells(board)))
    }
  }

  // Integration

  def replay(events: List[Event]): StateVersionPair = {

    def apply(game: Game, event: Event): Game = {
      (game, event) match {

        case (Uninitialized, GameStarted(_, board)) =>
          InProgress(board, 0)

        case (InProgress(_, totalScore), GroupRemoved(_, board, score)) =>
          InProgress(board, totalScore + score)

        case (InProgress(_, totalScore), FinalGroupRemoved(_, board, score)) =>
          Finished(board, totalScore + score)

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

  private def tryRemoveGroup(id: GameId, board: Board, position: Position): List[Event] = {
    play(board, position)
      .map {
        case (updatedBoard, score) =>
          evaluateGameState(updatedBoard, score) match {
            case InProgress(b, s) => List(GroupRemoved(id, b, s))
            case Finished(b, s)   => List(FinalGroupRemoved(id, b, s))
            case Uninitialized    => List() // impossible case
          }
      }
      .getOrElse(List())
  }

  def decide(id: GameId, game: Game, command: Command): Either[DomainMessage, List[Event]] = {
    (game, command) match {

      case (Uninitialized, StartNewGame(board)) =>
        Right(List(GameStarted(id, board)))

      case (InProgress(board, score), RemoveGroup(position)) =>
        Right(tryRemoveGroup(id, board, position))

      case (state, cmd) => Left(InvalidOperation(state, cmd))
    }
  }
}

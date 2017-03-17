package com.example.samegame

sealed trait Event
case class GameStarted(id: GameId, board: Board) extends Event
case class GroupRemoved(id: GameId, position: Position, board: Board, score: Int) extends Event
case class GameFinished(id: GameId) extends Event
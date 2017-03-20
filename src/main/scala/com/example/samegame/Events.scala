package com.example.samegame

sealed trait Event
final case class GameStarted(id: GameId, board: Board) extends Event
final case class GroupRemoved(id: GameId, board: Board, score: Int) extends Event
final case class GameFinished(id: GameId) extends Event
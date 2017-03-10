package com.example.samegame

sealed trait Event
case class GameStarted(id: GameId, board: Board) extends Event
case class GroupRemoved(id: GameId, board: Board, score: Int) extends Event
case class FinalGroupRemoved(id: GameId, board: Board, score: Int) extends Event
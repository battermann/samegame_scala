package com.example.samegame

sealed trait Event
final case class GameStarted(board: Board) extends Event
final case class GroupRemoved(position: Position, board: Board, score: Int) extends Event
object GameFinished extends Event

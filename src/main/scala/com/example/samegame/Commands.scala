package com.example.samegame

sealed trait Command
final case class StartNewGame(board: Board) extends Command
final case class RemoveGroup(position: Position) extends Command

package com.example.samegame

sealed trait Command
case class StartNewGame(board: Board) extends Command
case class RemoveGroup(position: Position) extends Command

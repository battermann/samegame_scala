package com.example.serialization

import com.example.samegame._
import julienrf.json.derived
import play.api.libs.json.Format

object implicits {
  implicit val gameIdFormat: Format[GameId] = derived.oformat[GameId]()
  implicit val positionFormat: Format[Position] = derived.oformat[Position]()
  implicit val colorFormat: Format[Color] = derived.oformat[Color]()
  implicit val cellStateFormat: Format[CellState] = derived.oformat[CellState]()
  implicit val cellFormat: Format[Cell] = derived.oformat[Cell]()
  implicit val columnFormat: Format[Column] = derived.oformat[Column]()
  implicit val boardFormat: Format[Board] = derived.oformat[Board]()
  implicit val eventFormat: Format[Event] = derived.oformat[Event]()
}

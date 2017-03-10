package com.example.samegame

sealed trait DomainMessage
case object BoardWithZeroColumnsNotValid extends DomainMessage
case object ColumnWithZeroCellsNotValid extends DomainMessage
case class InvalidOperation(state: Game, command: Command) extends DomainMessage
case class ConcurrencyFailure(msg: String) extends DomainMessage
case object BoardWithMultipleColumnHeightsNotValid extends DomainMessage
case class EventStoreAppendFailure(msg: String) extends DomainMessage

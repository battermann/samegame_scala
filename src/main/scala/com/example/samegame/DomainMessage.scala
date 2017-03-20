package com.example.samegame

sealed trait DomainMessage
case object BoardWithZeroColumnsNotValid extends DomainMessage
case object ColumnWithZeroCellsNotValid extends DomainMessage
final case class InvalidOperation(state: Game, command: Command) extends DomainMessage
final case class ConcurrencyFailure(msg: String) extends DomainMessage
case object BoardWithMultipleColumnHeightsNotValid extends DomainMessage
final case class EventStoreAppendFailure(msg: String) extends DomainMessage
case object InvalidGameIdFormat extends DomainMessage

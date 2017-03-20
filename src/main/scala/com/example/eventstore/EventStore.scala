package com.example.eventstore

import com.example.samegame.DomainMessage

import scala.concurrent.Future

final case class EventStore[Event](
  appendToStream: (String, Int, List[Event]) => Future[Either[DomainMessage, Unit]],
  readFromStream: String => Future[Either[DomainMessage, List[Event]]])

package com.example.eventstore

import com.example.eventstore.FutureEither.FutureEither

case class EventStore[Event](
  appendToStream: (String, Int, List[Event]) => FutureEither[Unit],
  readFromStream: String => FutureEither[List[Event]])

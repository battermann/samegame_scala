package com.example.eventstore

import cats.data._
import com.example.samegame.DomainMessage

import scala.concurrent.Future

object FutureEither {
  type FutureEither[A] = EitherT[Future, DomainMessage, A]
}

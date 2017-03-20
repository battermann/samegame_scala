package com.example.utils

import cats.data._
import com.example.samegame.DomainMessage

import scala.concurrent.Future

object Composition {
  type FutureEither[A] = EitherT[Future, DomainMessage, A]
}



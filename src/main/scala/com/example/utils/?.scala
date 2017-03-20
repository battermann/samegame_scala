package com.example.utils

import cats.data._
import cats.implicits._
import com.example.samegame.DomainMessage
import com.example.utils.Composition.FutureEither
import shapeless.<:!<

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ? {
  def <~[A](x: A): FutureEither[A] = x.pure[FutureEither]
  def <~[A](x: Either[DomainMessage, A]): FutureEither[A] = EitherT.fromEither[Future](x)
  def <~[A](x: Future[A])(implicit ev: A <:!< Either[DomainMessage, _]): FutureEither[A] = EitherT.right[Future, DomainMessage, A](x)
  def <~[A](x: Future[Either[DomainMessage, A]]): FutureEither[A] = EitherT(x)
}

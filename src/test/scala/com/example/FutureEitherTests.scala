package com.example

import org.scalatest.FunSuite

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global

object FutureEitherType {
  type FutureEither[A] = EitherT[Future, String, A]
}

import com.example.FutureEitherType.FutureEither

class FutureEitherTests extends FunSuite {

  test("FutureEither Tests") {
    val answer: FutureEither[Int] =
      for {
        a <- 10.pure[FutureEither]
        b <- 32.pure[FutureEither]
      } yield a + b

    val result = Await.result(answer.value, Duration.Inf)

    result.fold(
      err => fail(s"$err"),
      v => assert(v == 42)
    )
  }
}
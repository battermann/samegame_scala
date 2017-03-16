package com.example.eventstore

import java.nio.charset.StandardCharsets

import akka.actor.ActorSystem
import cats.data._
import cats.implicits._
import com.example.eventstore.FutureEither.FutureEither
import com.example.samegame.{ConcurrencyFailure, DomainMessage, Event, EventStoreAppendFailure}
import com.example.serialization.implicits._
import julienrf.json.derived
import org.joda.time.DateTimeUtils
import play.api.libs.json.{Format, Json}
import redis.RedisClient
import redis.api.scripting.RedisScript
import redis.protocol.MultiBulk

import scala.concurrent.{ExecutionContext, Future}

object RedisEventStore {
  def appendToStream(host: String, port: Int, commitsKey: String)(streamId: String, expectedVersion: Int, events: List[Event])
                             (implicit ec: ExecutionContext, as: ActorSystem): FutureEither[Unit] = {

    val redis = RedisClient(host = host, port = port)

    if(events.isEmpty) {
      ().pure[FutureEither]
    } else {

      val timestamp = DateTimeUtils.currentTimeMillis
      val serializedEvents = Json.stringify(Json.toJson(events))

      val tryCommitScript: String =
        """
        | local commitsKey = ARGV[1]
        | local timestamp = tonumber(ARGV[2])
        | local streamId = ARGV[3]
        | local expected = tonumber(ARGV[4])
        | local events = ARGV[5]
        |
        | local actual = tonumber(redis.call('llen', streamId)) - 1
        | if actual ~= expected then
        | return {'conflict', tostring(actual)}
        | end
        |
        | local storeRevision = tonumber(redis.call('hlen', commitsKey))
        | local commitId = storeRevision
        | local commitData = string.format('{"storeRevision":%d,"timestamp":%d,"streamId":%s,"streamRevision":%d,"events":%s}',
        | commitId, timestamp, cjson.encode(streamId), actual + 1, events)
        |
        | redis.call('hset', commitsKey, commitId, commitData)
        | redis.call('rpush', streamId, commitId)
        | redis.call('publish', commitsKey, commitData)
        |
        | return {'commit', tostring(commitId)}
      """.stripMargin

    val script = redis.
      evalshaOrEval(
        RedisScript(tryCommitScript), args = Seq(commitsKey, timestamp.toString, streamId, expectedVersion.toString, serializedEvents))

    val result: Future[Either[DomainMessage, Unit]] =
      script
        .map {
          case mb: MultiBulk =>
            mb.responses match {
              case Some(Vector(
              commit, commitId)) if commit.toByteString.utf8String == "commit" =>
                Right(
                )
              case Some(Vector(conflict, version)) if conflict.toByteString.utf8String == "conflict" =>
                Left(ConcurrencyFailure(s"conflict: expected version: $expectedVersion, but actual version: $version, streamId ($streamId)"))
              case _ =>
                Left(EventStoreAppendFailure("unknown error"))
            }
          case err => Left(EventStoreAppendFailure(s"$err"
          ))
        }
      EitherT(result)
      }
  }

  case class CommitData(storeRevision: Int, timestamp: Long, streamId: String, streamRevision: Int, events: Seq[Event])

  implicit val commitDataFormat: Format[CommitData] = derived.oformat[CommitData]()

  def readFromStream(host: String, port: Int, commitsKey: String)(streamId: String)
                             (implicit ec: ExecutionContext, as: ActorSystem): FutureEither[List[Event]] = {
    val redis = RedisClient(host = host, port = port)

    def readCommitData(commitIds: Seq[String]): Future[Seq[String]] = {
      commitIds match {
        case Nil => Future.successful(Seq())
        case _ =>
          redis.hmget(commitsKey, commitIds: _*)
            .map(_.flatten.map(_.decodeString(StandardCharsets.UTF_8)))
      }
    }

    val result = for {
      serIds <- redis.lrange(streamId, 0, -1)
      ids = serIds.map(_.decodeString(StandardCharsets.UTF_8))
      comData <- readCommitData(ids)
    } yield {
      comData
        .map(data => Json.parse(data).as[CommitData])
        .flatMap(_.events)
        .toList
    }

    EitherT.right(result)
  }
}

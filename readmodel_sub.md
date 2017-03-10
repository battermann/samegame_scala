# Read Model Code for subscribing to Redis pub channel

    package com.example

    import java.net.InetSocketAddress
    import java.nio.charset.StandardCharsets

    import akka.actor.Props
    import redis.RedisClient
    import redis.actors.RedisSubscriberActor
    import redis.api.pubsub.{Message, PMessage}

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    object ExamplePubSub extends App {
      implicit val akkaSystem = akka.actor.ActorSystem()
      val redis = RedisClient("localhost", 6379)
      akkaSystem.scheduler.scheduleOnce(20 seconds)(akkaSystem.terminate())
      val channels = Seq("samegame:commits")
      akkaSystem
        .actorOf(Props(classOf[SubscribeActor], channels, Nil)
        .withDispatcher("rediscala.rediscala-client-worker-dispatcher"))
    }

    class SubscribeActor(channels: Seq[String] = Nil, patterns: Seq[String] = Nil)
      extends RedisSubscriberActor(
        new InetSocketAddress("localhost", 6379),
        channels,
        patterns,
        onConnectStatus = connected => { println(s"connected: $connected")}
      ) {

      def onMessage(message: Message) {
        println(s" message received: ${message.data.decodeString(StandardCharsets.UTF_8)}")
      }

      def onPMessage(pmessage: PMessage) {
        println(s"pattern message received: $pmessage")
      }
    }
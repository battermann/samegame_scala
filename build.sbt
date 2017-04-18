name := """samegame"""

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "com.github.etaty" %% "rediscala" % "1.8.0"
libraryDependencies += "org.julienrf" %% "play-json-derived-codecs" % "4.0.0-RC1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"
name := "tztr"
scalaVersion := "2.12.8"

ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.boopboopbeepboop"
ThisBuild / organizationName := "boopboopbeepboop"

libraryDependencies ++= Seq(
  "com.jsuereth" %% "scala-arm" % "2.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)
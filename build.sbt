val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "WeatherAggregator",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "io.circe" %% "circe-core" % "0.15.0-M1",
      "io.circe" %% "circe-generic" % "0.15.0-M1",
      "io.circe" %% "circe-parser" % "0.15.0-M1"
    )
  )


ThisBuild / organization := "topaz"
ThisBuild / scalaVersion := "2.12.10"

val akkaV = "2.6.4"
val monocleV = "2.0.4"
val jacksonV = "2.10.3"

val commonsIO               = "commons-io"            % "commons-io"                  % "2.6"
val scalaCSV                = "com.github.tototoshi" %% "scala-csv"                   % "1.3.6"
val commonsLang             = "org.apache.commons"    % "commons-lang3"               % "3.10"
val commonsMath3            = "org.apache.commons"    % "commons-math3"               % "3.6.1"
val scalaTest               = "org.scalatest"        %% "scalatest"                   % "3.1.1"      % Test

def DepSeq(modules: ModuleID*): Seq[ModuleID] = {
  modules.map {
    _.exclude("org.slf4j", "slf4j-log4j12")
      .exclude("com.sun.jdmk", "jmxtools")
      .exclude("javax.jms", "jms")
      .exclude("com.sun.jmx", "jmxri")
      .exclude("commons-logging", "commons-logging")
  }
}

lazy val root = (project in file("."))
  .settings(
    name := "Topaz-Quantity",
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "tests",
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "net.logstash.logback" % "logstash-logback-encoder" % "6.3"
    ) ++ DepSeq(
      "com.typesafe.akka" %% "akka-actor" % akkaV,
      "com.typesafe.akka" %% "akka-slf4j" % akkaV,
      "com.typesafe.akka" %% "akka-stream" % akkaV,
      "com.typesafe.akka" %% "akka-remote" % akkaV,
      "nl.grons" %% "metrics-scala" % "4.0.0",
      "org.scalaz" %% "scalaz-core" % "7.2.30",
      "com.chuusai" %% "shapeless" % "2.3.3",
      commonsIO, commonsLang, commonsMath3, scalaCSV,
      "com.fasterxml.jackson.core" % "jackson-databind" % jacksonV,
      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % jacksonV,
      "com.beachape" %% "enumeratum" % "1.5.15",
      "com.github.ben-manes.caffeine" % "caffeine" % "2.8.1",
      "com.danielflower.apprunner" % "javasysmon" % "0.3.5.1", // for finding PIDs
      "net.s_mach" %% "datadiff" % "1.1.1", // for diffing/patching data structures
      "com.github.julien-truffaut" %%  "monocle-core"  % monocleV,
      "com.github.julien-truffaut" %%  "monocle-macro" % monocleV,
      "com.github.julien-truffaut" %%  "monocle-law"   % monocleV % Test,
      "com.typesafe.akka" %% "akka-stream-testkit" % akkaV % Test,
      scalaTest,
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % Test
    ),
  )

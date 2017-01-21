//crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.1") // meta is currently only for 2.11
scalaVersion in ThisBuild := "2.11.8"

val groupId = "com.github.thangiee"
name := "freasy-monad"
organization := groupId

lazy val root = project.in(file("."))
  .aggregate(js, jvm)
  .settings(
    publish := {},
    publishLocal := {}
  )

val nexus = "https://oss.sonatype.org/"

lazy val shared = crossProject.in(file("."))
  .settings(
    name := "shared",
    version := "0.1-SNAPSHOT",
    version := "0.5.0",
    scalacOptions ++= Seq(
      "-feature",
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfuture",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused-import",
      "-unchecked",
      "-Xplugin-require:macroparadise"
    ),
    resolvers += "snapshots" at nexus + "content/repositories/snapshots",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % "0.8.1" % "provided",
      "org.scalaz" %% "scalaz-core" % "7.2.7" % "provided",
      "org.scala-lang" % "scala-reflect" % "2.11.8",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test",
      "com.github.mpilquist" %% "simulacrum" % "0.10.0",
      "org.scalameta" %% "scalameta"   % "1.4.0"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-beta4" cross CrossVersion.full),
    sonatypeProfileName := groupId,
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else                  Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    publishArtifact in Test := false,
    pomExtra :=
      <url>https://github.com/Thangiee/Freasy-Monad</url>
        <licenses>
          <license>
            <name>MIT license</name>
            <url>http://www.opensource.org/licenses/mit-license.php</url>
          </license>
        </licenses>
        <scm>
          <url>git://github.com/Thangiee/Freasy-Monad.git</url>
          <connection>scm:git://github.com/Thangiee/Freasy-Monad.git</connection>
        </scm>
        <developers>
          <developer>
            <id>Thangiee</id>
            <name>Thang Le</name>
            <url>https://github.com/Thangiee</url>
          </developer>
        </developers>
  )
  .jvmSettings()
  .jsSettings()

lazy val jvm = shared.jvm
lazy val js = shared.js

addCommandAlias("packageLocalCore", ";coreJS/publishLocal;coreJVM/publishLocal")
addCommandAlias("publishCore", ";coreJS/publishSigned;coreJVM/publishSigned")
addCommandAlias("releaseCore", s"sonatypeReleaseAll $groupId")
//crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.1") // meta is currently only for 2.11
scalaVersion in ThisBuild := "2.11.8"

lazy val root = project.in(file("."))
  .aggregate(js, jvm)
  .settings(publishArtifact := false)

val nexus = "https://oss.sonatype.org/"
val groupId = "com.github.thangiee"

lazy val shared = crossProject.in(file("."))
  .settings(
    name := "freasy-monad",
    version := "0.6.0-SNAPSHOT",
    organization := groupId,
    scalacOptions ++= Seq(
      "-feature",
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfuture",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
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
    publishSettings
  )
  .jvmSettings()
  .jsSettings()

lazy val jvm = shared.jvm
lazy val js = shared.js

lazy val publishSettings = Seq(
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil, // macroparadise doesn't work with scaladoc yet.
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

addCommandAlias("publishFreasyMonad", "publishSigned")
addCommandAlias("releaseFreasyMonad", s"sonatypeReleaseAll $groupId")
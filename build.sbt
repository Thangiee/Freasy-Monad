onLoad in Global := ((s: State) => { "updateIdea" :: s}) compose (onLoad in Global).value

crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.0")

val groupId = "com.github.thangiee"
lazy val commonSettings = Seq(
  organization := groupId,
  scalaVersion in ThisBuild := "2.11.8"
)

val libVer = "0.5.0"
lazy val core = crossProject
  .settings(commonSettings)
  .settings(
    name := "freasy-monad",
    version := libVer,
    scalacOptions ++= Seq(
      "-feature",
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfuture",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused",
      "-Ywarn-unused-import",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats" % "0.8.1" % "provided",
      "org.scalaz" %%% "scalaz-core" % "7.2.7" % "provided",
      "org.scala-lang" % "scala-reflect" % "2.11.8",
      "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
  .settings(
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

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

addCommandAlias("packageLocalCore", ";coreJS/publishLocal;coreJVM/publishLocal")
addCommandAlias("publishCore", ";coreJS/publishSigned;coreJVM/publishSigned")
addCommandAlias("releaseCore", s"sonatypeReleaseAll $groupId")

val pluginVer = "0.5.1"
val pluginName = "freasy-monad-plugin"
lazy val plugin: Project = project
  .enablePlugins(SbtIdeaPlugin)
  .settings(commonSettings)
  .settings(
    name := pluginName,
    version := pluginVer,
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false),
    ideaInternalPlugins := Seq(),
    ideaExternalPlugins := Seq(IdeaPlugin.Zip("scala-plugin", url("https://plugins.jetbrains.com/plugin/download?pr=&updateId=29035"))),
    aggregate in updateIdea := false,
    assemblyExcludedJars in assembly := ideaFullJars.value,
    ideaBuild := "163.7743.17"
  )

lazy val ideaRunner: Project = project.in(file("ideaRunner"))
  .dependsOn(plugin % Provided)
  .settings(commonSettings)
  .settings(
    name := "ideaRunner",
    autoScalaLibrary := false,
    unmanagedJars in Compile := ideaMainJars.in(plugin).value,
    unmanagedJars in Compile += file(System.getProperty("java.home")).getParentFile / "lib" / "tools.jar"
  )

lazy val packagePlugin = TaskKey[File]("package-plugin", "Create plugin's zip file ready to load into IDEA")

packagePlugin in plugin := {
  val ideaJar = (assembly in plugin).value
  val sources = Seq(ideaJar -> s"$pluginName/lib/${ideaJar.getName}")
  val out = plugin.base / "bin" / s"$pluginName-$pluginVer.zip"
  IO.zip(sources, out)
  out
}
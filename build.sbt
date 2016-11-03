onLoad in Global := ((s: State) => { "updateIdea" :: s}) compose (onLoad in Global).value

crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.0")

lazy val commonSettings = Seq(
  organization := "com.thangiee",
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
      "org.typelevel" %%% "cats" % "0.8.0" % "provided",
      "org.scalaz" %%% "scalaz-core" % "7.2.6" % "provided",
      "org.scala-lang" % "scala-reflect" % "2.11.8",
      "org.scalatest" %%% "scalatest" % "3.0.0" % "test"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
  .settings(
    publishMavenStyle := true,
    bintrayReleaseOnPublish in ThisBuild := false, //  1."sbt core/publish" stage artifacts first 2."sbt core/bintrayRelease" make artifacts public
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    bintrayVcsUrl := Some("https://github.com/Thangiee/Freasy-Monad"),
    if (libVer.endsWith("-SNAPSHOT"))
      Seq(
        publishTo := Some("Artifactory Realm" at "http://oss.jfrog.org/artifactory/oss-snapshot-local"),
        // Only setting the credentials file if it exists (#52)
        credentials := List(Path.userHome / ".bintray" / ".artifactory").filter(_.exists).map(Credentials(_))
      )
    else Seq.empty
  )
  .enablePlugins(ScalaJSPlugin)
  .jvmSettings()
  .jsSettings()

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

val pluginVer = "0.5.0"
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
    assemblyExcludedJars in assembly <<= ideaFullJars,
    ideaBuild := "163.6957.12"
  )

lazy val ideaRunner: Project = project.in(file("ideaRunner"))
  .dependsOn(plugin % Provided)
  .settings(commonSettings)
  .settings(
    name := "ideaRunner",
    autoScalaLibrary := false,
    unmanagedJars in Compile <<= ideaMainJars.in(plugin),
    unmanagedJars in Compile += file(System.getProperty("java.home")).getParentFile / "lib" / "tools.jar"
  )

lazy val packagePlugin = TaskKey[File]("package-plugin", "Create plugin's zip file ready to load into IDEA")

packagePlugin in plugin <<= (assembly in plugin, ivyPaths) map { (ideaJar, paths) =>
  val ivyLocal = paths.ivyHome.getOrElse(file(System.getProperty("user.home")) / ".ivy2") / "local"
  val sources = Seq(ideaJar -> s"$pluginName/lib/${ideaJar.getName}")
  val out = plugin.base / "bin" / s"$pluginName-$pluginVer.zip"
  IO.zip(sources, out)
  out
}
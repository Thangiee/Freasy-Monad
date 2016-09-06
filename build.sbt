onLoad in Global := ((s: State) => { "updateIdea" :: s}) compose (onLoad in Global).value

lazy val commonSettings = Seq(
  organization := "com.thangiee",
  scalaVersion := "2.11.8"
)

lazy val core = project
  .settings(commonSettings)
  .settings(
    name := "freasy-monad",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-feature",
      "-encoding", "UTF-8",
      "-unchecked",
      "-deprecation",
      "-Xfuture",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-unused",
      "-Ywarn-unused-import",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats" % "0.7.2",
      "org.scala-lang" % "scala-reflect" % "2.11.8"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
  .settings(
    publishMavenStyle := true,
    bintrayReleaseOnPublish in ThisBuild := false, //  1."sbt core/publish" stage artifacts first 2."sbt core/bintrayRelease" make artifacts public
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    bintrayVcsUrl := Some("https://github.com/Thangiee/Freasy-Monad")
  )

val pluginVer = "0.1.0"
val pluginName = "freasy-monad-plugin"
lazy val plugin: Project = project
  .enablePlugins(SbtIdeaPlugin)
  .settings(commonSettings)
  .settings(
    name := pluginName,
    version := pluginVer,
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false),
    ideaInternalPlugins := Seq(),
    ideaExternalPlugins := Seq(IdeaPlugin.Zip("scala-plugin", url("https://plugins.jetbrains.com/plugin/download?pr=&updateId=27915"))),
    aggregate in updateIdea := false,
    assemblyExcludedJars in assembly <<= ideaFullJars,
    ideaBuild := "163.3512.7"
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
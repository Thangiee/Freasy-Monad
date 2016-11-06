logLevel := Level.Warn

resolvers += Resolver.url("dancingrobot84-bintray",
  url("http://dl.bintray.com/dancingrobot84/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.dancingrobot84" % "sbt-idea-plugin" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.13")
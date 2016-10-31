logLevel := Level.Warn

addSbtPlugin("org.scala-android" % "sbt-android" % "1.6.10")

resolvers += Resolver.url("dancingrobot84-bintray",
  url("http://dl.bintray.com/dancingrobot84/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.dancingrobot84" % "sbt-idea-plugin" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.13")
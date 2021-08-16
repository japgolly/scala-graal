libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.9.30")
addSbtPlugin("com.github.sbt"     % "sbt-pgp"                  % "2.1.2")
addSbtPlugin("com.github.sbt"     % "sbt-release"              % "1.1.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.6.0")
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                 % "2.2.21")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"             % "3.9.7")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.3")

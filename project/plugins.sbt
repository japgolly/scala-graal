libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.9.34")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.5.12")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.11.0")
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                 % "2.3.6")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.5")

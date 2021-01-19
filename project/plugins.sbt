val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).filter(_.nonEmpty).getOrElse("1.1.1")

libraryDependencies ++= {
  if (scalaJSVersion.startsWith("0."))
    Nil
  else
    Seq("org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0")
}

addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.9.24")
addSbtPlugin("com.github.gseitz"  % "sbt-release"              % "1.0.13")
addSbtPlugin("com.jsuereth"       % "sbt-pgp"                  % "1.1.2-1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                  % "1.0.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % scalaJSVersion)
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                 % "2.2.16")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"             % "3.9.5")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.0")

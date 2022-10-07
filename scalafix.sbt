ThisBuild / scalafixScalaBinaryVersion := "2.13"
ThisBuild / semanticdbEnabled          := true
ThisBuild / semanticdbVersion          := "4.6.0"

ThisBuild / scalacOptions ++= {
  if (scalaVersion.value startsWith "2")
    "-Yrangepos" :: Nil
  else
    Nil
}

ThisBuild / scalafixDependencies ++= Seq(
  "com.github.liancheng" %% "organize-imports" % "0.6.0"
)

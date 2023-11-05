scalaVersion := "2.13.1"

lazy val hello = (project in file("."))
  .settings(
    name := "learning-cats"
  )


addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
libraryDependencies += "org.typelevel" %% "cats-laws" % "2.9.0"
libraryDependencies += "org.typelevel" %% "discipline-core" % "1.5.1"
libraryDependencies += "org.typelevel" %% "discipline-scalatest" % "2.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15"
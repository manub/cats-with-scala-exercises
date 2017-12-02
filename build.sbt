name := "scala-with-cats-exercises"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-Ypartial-unification", "-language:higherKinds")

libraryDependencies ++=
  Seq(
    "org.typelevel" %% "cats-core" % "1.0.0-RC1",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  )

lazy val root = (project in file(".")).
  settings(
    name         := "typequest",
    organization := "typequest",
    version      := "0.1.0",
    scalaVersion := "2.11.7"
  )

libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats"           % "0.4.1",
  "com.chuusai"    %% "shapeless"      % "2.3.0",
  "org.scala-lang"  % "scala-compiler" % "2.11.7"
)

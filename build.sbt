import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.0",
    libraryDependencies += "commons-io" % "commons-io" % "2.5",
    libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.1",
    libraryDependencies += "junit" % "junit" % "4.12",
    libraryDependencies += "net.liftweb" %% "lift-json" % "3.0.1"
  )

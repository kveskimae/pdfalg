import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.pdfextractor",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),

    name := "PDF Extractor Algorithm",

    resolvers += Resolver.mavenLocal,

    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.0",
    libraryDependencies += "commons-configuration" % "commons-configuration" % "1.10" % Test,
    libraryDependencies += "commons-io" % "commons-io" % "2.5",
    libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.1",
    libraryDependencies += "log4j" % "log4j" % "1.2.17",
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.21",
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.21",
    libraryDependencies += "net.liftweb" %% "lift-json" % "3.0.1",
    libraryDependencies += "org.pdfextractor" % "db" % "3",
    libraryDependencies += "org.springframework" % "spring-context-support" % "4.2.6.RELEASE",
    libraryDependencies += "org.springframework" % "spring-test" % "4.2.6.RELEASE" % Test

  )

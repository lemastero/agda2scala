name := "exampleScala2"

version := "0.0.1"

lazy val scala212 = "2.12.18"
lazy val scala213 = "2.13.12"

scalaVersion := scala213

crossScalaVersions := List(scala212, scala213)

resolvers += Resolver.sonatypeRepo("snapshots")

lazy val zioVersion = "2.0.22"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
  "dev.zio" %% "zio-test-junit" % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia" % zioVersion % Test
)

scalacOptions ++= Seq(
  "-encoding", "UTF-8"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat

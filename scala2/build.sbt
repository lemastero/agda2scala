name := "exampleScala2"

version := "0.0.1"

scalaVersion := "2.13.15"

resolvers ++= Resolver.sonatypeOssRepos("snapshots")

lazy val zioVersion = "2.1.11"

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

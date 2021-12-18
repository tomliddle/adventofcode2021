val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventofcode",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.3.0",
      "org.typelevel" %% "cats-effect-kernel" % "3.3.0",
      "org.typelevel" %% "cats-effect-std" % "3.3.0",
      "org.typelevel" %% "cats-effect-testing-specs2" % "1.4.0" % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
    )
  )

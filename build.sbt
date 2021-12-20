ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file(".")).settings(
  name := "advent-of-code-2021",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect"         % "3.3.0",
    "org.typelevel" %% "cats-effect-kernel"  % "3.3.0",
    "org.typelevel" %% "cats-effect-std"     % "3.3.0",
    "org.typelevel" %% "cats-parse"          % "0.3.6",
    "co.fs2"        %% "fs2-core"            % "3.2.2",
    "co.fs2"        %% "fs2-io"              % "3.2.2",
    "eu.timepit"    %% "refined"             % "0.9.28",
    "eu.timepit"    %% "refined-cats"        % "0.9.28",
    "com.beachape"  %% "enumeratum"          % "1.7.0",
    "io.estatico"   %% "newtype"             % "0.4.4",
    "org.scodec"    %% "scodec-bits"         % "1.1.30",
    "org.scodec"    %% "scodec-core"         % "1.11.9",
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.6" % Test,
  ),
  addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1"),
  addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full),
  scalacOptions += "-Ymacro-annotations",
  scalacOptions += "-Ywarn-macros:after",
)

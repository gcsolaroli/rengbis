// ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("snapshots")

ThisBuild / organization  := "relax-schema"
ThisBuild / scalaVersion  := "3.7.4"
ThisBuild / usePipelining := true

ThisBuild / scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xcheck-macros",
    "-Yexplicit-nulls", // experimental (I've seen it cause issues with circe)
    "-Xkind-projector",
    "-Wsafe-init"       // experimental (I've seen it cause issues with circe)
) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future")

lazy val root =
    project
        .in(file("."))
        .enablePlugins(NativeImagePlugin)
        .settings(name := "rengbis")
        .settings(version := "0.0.1-SNAPSHOT")
        .settings(dependencies)
        .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
        .settings(
            assembly / mainClass             := Some("rengbis.Main"),
            assembly / assemblyJarName       := "rengbis.jar",
            assembly / assemblyMergeStrategy := {
                case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
                case PathList("META-INF", xs @ _*)             => MergeStrategy.discard
                case x if x.endsWith("module-info.class")      => MergeStrategy.discard
                case x                                         => MergeStrategy.first
            },
            Compile / mainClass              := Some("rengbis.Main"),
            nativeImageInstalled             := true, // Use system-installed native-image from GRAALVM_HOME or PATH
            nativeImageOptions ++= Seq(
                "--no-fallback",
                "-H:+ReportExceptionStackTraces"
            )
        )

// lazy val commonScalacOptions = Seq(
//   Compile / console / scalacOptions --= Seq(
//     "-Wunused:_",
//     "-Xfatal-warnings",
//   ),
//   Test / console / scalacOptions :=
//     (Compile / console / scalacOptions).value,
// )

// lazy val zioSchemaAvro = ProjectRef(file("../scala/zio/zio-schema"), "zio-schema")

val zio            = "2.1.16"
val zio_json       = "0.7.39"
val zio_parser     = "0.1.11"
val zio_cli        = "0.7.4"
val yaml4s_version = "0.3.0"

lazy val dependencies = Seq(
    libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-xml"       % "2.3.0",
        "dev.zio"                %% "zio"             % zio,
        "dev.zio"                %% "zio-json"        % zio_json,
        "dev.zio"                %% "zio-parser"      % zio_parser,
        "dev.zio"                %% "zio-cli"         % zio_cli,
        "dev.hnaderi"            %% "yaml4s-backend"  % yaml4s_version,
        "dev.hnaderi"            %% "yaml4s-zio-json" % yaml4s_version,
        "org.slf4j"               % "slf4j-simple"    % "2.0.13"
    ),
    libraryDependencies ++= Seq(
        "dev.zio" %% "zio-test"     % zio,
        "dev.zio" %% "zio-test-sbt" % zio
        // "org.scalatest"         %% "scalatest"            % "3.2.19",     //  https://www.baeldung.com/scala/scalatest
    ).map(_ % Test)
)

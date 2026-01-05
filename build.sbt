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

// Dependency versions
val zio            = "2.1.16"
val zio_json       = "0.7.39"
val zio_parser     = "0.1.11"
val zio_cli        = "0.7.4"
val yaml4s_version = "0.3.0"
val lsp4j_version  = "0.23.1"

// Root project - aggregates all modules
lazy val root = project
    .in(file("."))
    .settings(
        name           := "rengbis-root",
        version        := "0.0.1-SNAPSHOT",
        publish / skip := true
    )
    .aggregate(core, lsp)

// Core module - schema parser, validator, and data parsers
lazy val core = project
    .in(file("modules/core"))
    .enablePlugins(NativeImagePlugin)
    .settings(
        name    := "rengbis-core",
        version := "0.0.1-SNAPSHOT"
    )
    .settings(coreDependencies)
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
        nativeImageInstalled             := true,
        nativeImageOptions ++= Seq(
            "--no-fallback",
            "-H:+ReportExceptionStackTraces"
        )
    )

// LSP module - Language Server Protocol implementation
lazy val lsp = project
    .in(file("modules/lsp"))
    .dependsOn(core)
    .settings(
        name    := "rengbis-lsp",
        version := "0.0.1-SNAPSHOT"
    )
    .settings(lspDependencies)
    .settings(
        assembly / mainClass             := Some("rengbis.lsp.Main"),
        assembly / assemblyJarName       := "rengbis-lsp.jar",
        assembly / assemblyMergeStrategy := {
            case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
            case PathList("META-INF", xs @ _*)             => MergeStrategy.discard
            case x if x.endsWith("module-info.class")      => MergeStrategy.discard
            case x                                         => MergeStrategy.first
        },
        Compile / mainClass              := Some("rengbis.lsp.Main")
    )

lazy val coreDependencies = Seq(
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
    ).map(_ % Test)
)

lazy val lspDependencies = Seq(
    libraryDependencies ++= Seq(
        "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % lsp4j_version
    )
)

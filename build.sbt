import scala.sys.process._

ThisBuild / organization  := "rengbis"
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

val RENGBIS_VERSION = "0.0.1"

val zio            = "2.1.24"
val zio_json       = "0.7.45"
val zio_parser     = "0.1.11"
val zio_cli        = "0.7.4"
val yaml4s_version = "0.3.2"
// val tototoshi_csv_version = "2.0.0"

lazy val execjar = taskKey[File]("Create executable JAR using execjar tool")

lazy val root = project
    .in(file("."))
    .settings(
        name           := "rengbis-root",
        version        := RENGBIS_VERSION,
        publish / skip := true
    )
    .aggregate(core, cli)
    .settings(
        // Shortcuts: assembly for core (library JAR), execjar/nativeImage for cli (executables)
        assembly / aggregate    := false,
        assembly                := (core / assembly).value,
        execjar / aggregate     := false,
        execjar                 := (cli / execjar).value,
        nativeImage / aggregate := false,
        nativeImage             := (cli / nativeImage).value
    )

lazy val core = project
    .in(file("modules/core"))
    .settings(
        name    := "rengbis-core",
        version := RENGBIS_VERSION
    )
    .settings(coreDependencies)
    .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
    .settings(
        assembly / assemblyJarName       := "rengbis.jar",
        assembly / assemblyMergeStrategy := {
            case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
            case PathList("META-INF", xs @ _*)             => MergeStrategy.discard
            case x if x.endsWith("module-info.class")      => MergeStrategy.discard
            case x                                         => MergeStrategy.first
        }
    )

lazy val cli = project
    .in(file("modules/cli"))
    .dependsOn(core)
    .enablePlugins(NativeImagePlugin, BuildInfoPlugin)
    .settings(
        name    := "rengbis-cli",
        version := RENGBIS_VERSION
    )
    .settings(
        buildInfoKeys    := Seq[BuildInfoKey](
            name,
            version,
            BuildInfoKey.action("gitCommit") {
                scala.util.Try("git rev-parse --short HEAD".!!.trim).getOrElse("unknown")
            },
            BuildInfoKey.action("gitDirty") {
                scala.util.Try("git status --porcelain".!!.trim.nonEmpty).getOrElse(false)
            }
        ),
        buildInfoPackage := "rengbis.cli"
    )
    .settings(cliDependencies)
    .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
    .settings(
        Compile / mainClass              := Some("rengbis.cli.Main"),
        assembly / mainClass             := Some("rengbis.cli.Main"),
        assembly / assemblyJarName       := "rengbis-cli-assembly.jar",
        assembly / assemblyMergeStrategy := {
            case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
            case PathList("META-INF", xs @ _*)             => MergeStrategy.discard
            case x if x.endsWith("module-info.class")      => MergeStrategy.discard
            case x                                         => MergeStrategy.first
        },
        nativeImageInstalled             := true,
        nativeImageOptions ++= Seq(
            "--no-fallback",
            "-H:+ReportExceptionStackTraces"
        ),
        execjar                          := {
            val log        = streams.value.log
            val jarFile    = assembly.value
            val outputFile = target.value / "execjar" / "rengbis-cli"
            val minJavaVer = "17"

            log.info(s"Creating executable JAR from ${ jarFile.getName }...")
            // Download and run execjar directly from GitHub releases
            val execjarVersion = "v0.1.1"
            val execjarUrl     = s"https://github.com/parttimenerd/execjar/releases/download/$execjarVersion/execjar.jar"

            val execjarCmd = Seq("jbang", execjarUrl, jarFile.getAbsolutePath, "-o", outputFile.getAbsolutePath, "--min-java-version", minJavaVer)

            val result = execjarCmd.!
            if (result != 0) { sys.error(s"execjar failed with exit code $result. Ensure jbang is installed and available in PATH.") }
            s"chmod +x ${ outputFile.getAbsolutePath }".!

            log.info(s"Executable JAR created at: $outputFile")
            outputFile
        }
    )

lazy val coreDependencies = Seq(
    libraryDependencies ++= Seq(
        "org.scala-lang.modules" %% "scala-xml"       % "2.3.0",
        "dev.zio"                %% "zio"             % zio,
        "dev.zio"                %% "zio-json"        % zio_json,
        "dev.zio"                %% "zio-parser"      % zio_parser,
        "dev.hnaderi"            %% "yaml4s-backend"  % yaml4s_version,
        "dev.hnaderi"            %% "yaml4s-zio-json" % yaml4s_version
        // "com.github.tototoshi"   %% "scala-csv"       % tototoshi_csv_version
    ),
    libraryDependencies ++= Seq(
        "dev.zio" %% "zio-test"     % zio,
        "dev.zio" %% "zio-test-sbt" % zio
    ).map(_ % Test)
)

lazy val cliDependencies = Seq(
    libraryDependencies ++= Seq(
        "dev.zio"  %% "zio-cli"      % zio_cli,
        "org.slf4j" % "slf4j-simple" % "2.0.13"
    ),
    libraryDependencies ++= Seq(
        "dev.zio" %% "zio-test"     % zio,
        "dev.zio" %% "zio-test-sbt" % zio
    ).map(_ % Test)
)

package rengbis.cli

import rengbis.{ DataParsers, SchemaLoader, Validator }
import zio.cli.HelpDoc.Span.text

import java.nio.file.{ Files, Path, Paths }
import zio.{ Console, ZIO }
import zio.cli.Options
import zio.cli.{ Args, CliApp, Command, Exists, HelpDoc, ZIOCliDefault }
import java.util.logging.LogManager

object Main extends ZIOCliDefault:

    enum Format:
        case Json, Yaml, Xml

    enum RengbisCommand:
        case ValidateSchema(schemaFiles: List[Path])
        case ValidateData(format: Format, schemaFile: Path, schema: Option[String], dataFiles: List[Path])

    val formatOption: Options[Format] =
        Options
            .enumeration[Format]("format")(
                "json" -> Format.Json,
                "yaml" -> Format.Yaml,
                "xml"  -> Format.Xml
            )
            .alias("f")

    def schemaFileOption(exists: Exists = Exists.Yes): Options[Path] =
        Options
            .file("schema", exists)
            .alias("s") ?? "Path to the rengbis schema file"

    def schemaNameOption: Options[Option[String]] =
        Options.text("name").alias("n").optional

    def filesArg(exists: Exists = Exists.Yes): Args[List[Path]] =
        Args.file("files", exists).+ ?? "One or more files to validate"

    val validateSchemaHelp: HelpDoc =
        HelpDoc.p("Validate one or more rengbis schema files for syntax errors.")

    val validateDataHelp: HelpDoc =
        HelpDoc.p("Validate data files against a rengbis schema.")

    def validateSchemaCommand(exists: Exists = Exists.Yes): Command[RengbisCommand] =
        Command("validate-schema", Options.none, filesArg(exists))
            .withHelp(validateSchemaHelp)
            .map { files => RengbisCommand.ValidateSchema(files) }

    def validateDataCommand(exists: Exists = Exists.Yes): Command[RengbisCommand] =
        Command("validate-data", formatOption ++ schemaFileOption(exists) ++ schemaNameOption, filesArg(exists))
            .withHelp(validateDataHelp)
            .map { case ((format, schemaPath, schemaName), dataFiles) =>
                RengbisCommand.ValidateData(format, schemaPath, schemaName, dataFiles)
            }

    def buildCommand(exists: Exists): Command[RengbisCommand] =
        Command("rengbis")
            .withHelp(HelpDoc.p("ReNGBis - A content schema definition language for validating payloads."))
            .subcommands(validateSchemaCommand(exists), validateDataCommand(exists))

    val command: Command[RengbisCommand] = buildCommand(Exists.Yes)

    val versionString: String =
        val snapshot = if BuildInfo.gitDirty then " - SNAPSHOT" else ""
        s"${ BuildInfo.version } [${ BuildInfo.gitCommit }$snapshot]"

    val cliApp: CliApp[Any, Throwable, Unit] =
        CliApp.make(
            name = "rengbis",
            version = versionString,
            summary = text("Validate rengbis schema and data files"),
            command = command
        )(execute)

    def execute(cmd: RengbisCommand): ZIO[Any, Throwable, Unit] =
        cmd match
            case RengbisCommand.ValidateSchema(schemaFiles)                         => validateSchemas(schemaFiles)
            case RengbisCommand.ValidateData(format, schemaFile, schema, dataFiles) => validateData(format, schemaFile, schema, dataFiles)

    def validateSchemas(paths: List[Path]): ZIO[Any, Throwable, Unit] =
        ZIO.foreach(paths) { path =>
            val result = SchemaLoader.loadSchemaAtPath(path)
            result match
                case Right(_)    =>
                    Console.printLine(s"✓ ${ path.getFileName }: valid")
                case Left(error) =>
                    Console.printLine(s"✗ ${ path.getFileName }: $error")
            ZIO.succeed(result.isRight)
        }.flatMap { results =>
            val total   = results.size
            val valid   = results.count(identity)
            val invalid = total - valid
            Console.printLine(s"\nSummary: $valid valid, $invalid invalid out of $total schema(s)") *>
                ZIO.when(invalid > 0)(ZIO.fail(new RuntimeException(s"$invalid schema(s) failed validation")))
        }.unit

    def validateData(format: Format, schemaFile: Path, schema: Option[String], dataFiles: List[Path]): ZIO[Any, Throwable, Unit] =
        for
            loadedSchema                    <- ZIO.fromEither(SchemaLoader.loadSchemaAtPath(schemaFile)).mapError(e => new RuntimeException(s"Failed to parse schema ${ schemaFile.getFileName }: $e"))
            selectedSchema                   = schema match
                                                   case Some(s) => loadedSchema.definitions(s)
                                                   case None    => loadedSchema.root.get
            // parser: DataParsers.FileParser = format match
            parser: DataParsers.Parser[Path] = format match
                                                   case Format.Json => DataParsers.json
                                                   case Format.Yaml => DataParsers.yaml
                                                   case Format.Xml  => DataParsers.xml
            results                         <- ZIO.foreach(dataFiles) { file =>
                                                   val result = Validator.validate(parser(file))(selectedSchema)

                                                   if result.isValid then Console.printLine(s"✓ ${ file.getFileName }: valid")
                                                   else Console.printLine(s"✗ ${ file.getFileName }:\n${ result.errorMessage }")
                                                   ZIO.succeed(result.isValid)
                                               }
            total                            = results.size
            valid                            = results.count(identity)
            invalid                          = total - valid
            _                               <- Console.printLine(s"\nSummary: $valid valid, $invalid invalid out of $total file(s)")
            _                               <- ZIO.when(invalid > 0)(ZIO.fail(new RuntimeException(s"$invalid file(s) failed validation")))
        yield ()

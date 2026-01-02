package rengbis

import zio.cli.HelpDoc.Span.text

import java.nio.file.{ Files, Path }
import zio.{ Console, ZIO }
import zio.cli.Options
import zio.cli.{ Args, CliApp, Command, Exists, HelpDoc, ZIOCliDefault }

object Main extends ZIOCliDefault:

    enum Format:
        case Json, Yaml, Xml

    enum CliCommand:
        case ValidateSchema(schemaFiles: List[Path])
        case ValidateData(format: Format, schemaFile: Path, dataFiles: List[Path])

    val formatOption: Options[Format] =
        Options
            .enumeration[Format]("format")(
                "json" -> Format.Json,
                "yaml" -> Format.Yaml,
                "xml"  -> Format.Xml
            )
            .alias("f")

    val schemaOption: Options[Path] =
        Options
            .file("schema", Exists.Yes)
            .alias("s") ?? "Path to the rengbis schema file"

    val filesArg: Args[List[Path]] =
        Args.file("files", Exists.Yes).+ ?? "One or more files to validate"

    val validateSchemaHelp: HelpDoc =
        HelpDoc.p("Validate one or more rengbis schema files for syntax errors.")

    val validateDataHelp: HelpDoc =
        HelpDoc.p("Validate data files against a rengbis schema.")

    val validateSchemaCommand: Command[CliCommand] =
        Command("validate-schema", Options.none, filesArg)
            .withHelp(validateSchemaHelp)
            .map { files => CliCommand.ValidateSchema(files) }

    val validateDataCommand: Command[CliCommand] =
        Command("validate-data", formatOption ++ schemaOption, filesArg)
            .withHelp(validateDataHelp)
            .map { case ((format, schemaFile), dataFiles) =>
                CliCommand.ValidateData(format, schemaFile, dataFiles)
            }

    val command: Command[CliCommand] =
        Command("rengbis")
            .withHelp(HelpDoc.p("ReNGBis - A content schema definition language for validating payloads."))
            .subcommands(validateSchemaCommand, validateDataCommand)

    val cliApp: CliApp[Any, Throwable, Unit] =
        CliApp.make(
            name = "rengbis",
            version = "0.0.1",
            summary = text("Validate rengbis schemas and data files"),
            command = command
        )(execute)

    def execute(cmd: CliCommand): ZIO[Any, Throwable, Unit] =
        cmd match
            case CliCommand.ValidateSchema(schemaFiles)                 => validateSchemas(schemaFiles)
            case CliCommand.ValidateData(format, schemaFile, dataFiles) => validateData(format, schemaFile, dataFiles)

    def validateSchemas(files: List[Path]): ZIO[Any, Throwable, Unit] =
        ZIO.foreach(files) { file =>
            for
                content <- ZIO.attempt(Files.readString(file))
                result   = Schema.parse(content)
                _       <- result match
                               case Right(_)    =>
                                   Console.printLine(s"✓ ${ file.getFileName }: valid")
                               case Left(error) =>
                                   Console.printLine(s"✗ ${ file.getFileName }: $error")
            yield result.isRight
        }.flatMap { results =>
            val total   = results.size
            val valid   = results.count(identity)
            val invalid = total - valid
            Console.printLine(s"\nSummary: $valid valid, $invalid invalid out of $total schema(s)") *>
                ZIO.when(invalid > 0)(ZIO.fail(new RuntimeException(s"$invalid schema(s) failed validation")))
        }.unit

    def validateData(format: Format, schemaFile: Path, dataFiles: List[Path]): ZIO[Any, Throwable, Unit] =
        for
            schemaContent <- ZIO.attempt(Files.readString(schemaFile))
            schema        <- ZIO.fromEither(Schema.parse(schemaContent)).mapError(e => new RuntimeException(s"Failed to parse schema ${ schemaFile.getFileName }: $e"))
            parser         = format match
                                 case Format.Json => DataParsers.json
                                 case Format.Yaml => DataParsers.yaml
                                 case Format.Xml  => DataParsers.xml
            results       <- ZIO.foreach(dataFiles) { file =>
                                 for
                                     content <- ZIO.attempt(Files.readString(file))
                                     result   = Validator.validateString(parser)(schema, content)
                                     _       <-
                                         if result.isValid then Console.printLine(s"✓ ${ file.getFileName }: valid")
                                         else Console.printLine(s"✗ ${ file.getFileName }:\n${ result.errorMessage }")
                                 yield result.isValid
                             }
            total          = results.size
            valid          = results.count(identity)
            invalid        = total - valid
            _             <- Console.printLine(s"\nSummary: $valid valid, $invalid invalid out of $total file(s)")
            _             <- ZIO.when(invalid > 0)(ZIO.fail(new RuntimeException(s"$invalid file(s) failed validation")))
        yield ()

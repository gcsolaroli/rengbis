package rengbis.cli

import rengbis.{ DataParsers, SchemaLoader, SchemaSyntax, Validator }
import zio.cli.HelpDoc.Span.text

import java.nio.file.{ Files, Path, Paths }
import zio.{ Console, ZIO }
import zio.cli.Options
import zio.cli.{ Args, CliApp, Command, Exists, HelpDoc, ZIOCliDefault }
import java.util.logging.LogManager

object Main extends ZIOCliDefault:

    enum Format:
        case Json, Yaml, Xml, Csv

    enum SchemaFormat:
        case Xsd, JsonSchema, Avro

    enum RengbisCommand:
        case ValidateSchema(schemaFiles: List[Path])
        case ValidateData(format: Format, schemaFile: Path, schema: Option[String], dataFiles: List[Path])
        case TranslateSchemaFrom(format: SchemaFormat, source: Path, target: Option[Path], report: Option[Path])
        case TranslateSchemaTo(format: SchemaFormat, source: Path, target: Option[Path], report: Option[Path], rootName: Option[String])

    val formatOption: Options[Format] =
        Options
            .enumeration[Format]("format")(
                "json" -> Format.Json,
                "yaml" -> Format.Yaml,
                "xml"  -> Format.Xml,
                "csv"  -> Format.Csv
            )
            .alias("f")

    val schemaFormatOption: Options[SchemaFormat] =
        Options
            .enumeration[SchemaFormat]("format")(
                "xsd"        -> SchemaFormat.Xsd,
                "jsonschema" -> SchemaFormat.JsonSchema,
                "avro"       -> SchemaFormat.Avro
            )
            .alias("f") ?? "Schema format (xsd, jsonschema, avro)"

    def schemaFileOption(exists: Exists = Exists.Yes): Options[Path] =
        Options
            .file("schema", exists)
            .alias("s") ?? "Path to the rengbis schema file"

    def schemaNameOption: Options[Option[String]] =
        Options.text("name").alias("n").optional

    def sourceFileOption(exists: Exists = Exists.Yes): Options[Path] =
        Options
            .file("source", exists)
            .alias("s") ?? "Source file path"

    def targetFileOption: Options[Option[Path]] =
        Options
            .file("target", Exists.No)
            .alias("t")
            .optional ?? "Target file path (optional, defaults to stdout)"

    def reportFileOption: Options[Option[Path]] =
        Options
            .file("report", Exists.No)
            .alias("r")
            .optional ?? "Friction report file path (.md or .txt)"

    def rootNameOption: Options[Option[String]] =
        Options
            .text("root-name")
            .optional ?? "Root element name for XSD export (default: 'root')"

    def filesArg(exists: Exists = Exists.Yes): Args[List[Path]] =
        Args.file("files", exists).+ ?? "One or more files to validate"

    val validateSchemaHelp: HelpDoc =
        HelpDoc.p("Validate one or more rengbis schema files for syntax errors.")

    val validateDataHelp: HelpDoc =
        HelpDoc.p("Validate data files against a rengbis schema.")

    val translateSchemaFromHelp: HelpDoc =
        HelpDoc.p("Translate a schema from another format (XSD, JSON Schema) to ReNGBis format.")

    val translateSchemaToHelp: HelpDoc =
        HelpDoc.p("Translate a ReNGBis schema to another format (XSD, JSON Schema).")

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

    def translateSchemaFromCommand(exists: Exists = Exists.Yes): Command[RengbisCommand] =
        Command("translate-schema-from", schemaFormatOption ++ sourceFileOption(exists) ++ targetFileOption ++ reportFileOption)
            .withHelp(translateSchemaFromHelp)
            .map { case (format, source, target, report) =>
                RengbisCommand.TranslateSchemaFrom(format, source, target, report)
            }

    def translateSchemaToCommand(exists: Exists = Exists.Yes): Command[RengbisCommand] =
        Command("translate-schema-to", schemaFormatOption ++ sourceFileOption(exists) ++ targetFileOption ++ reportFileOption ++ rootNameOption)
            .withHelp(translateSchemaToHelp)
            .map { case (format, source, target, report, rootName) =>
                RengbisCommand.TranslateSchemaTo(format, source, target, report, rootName)
            }

    def buildCommand(exists: Exists): Command[RengbisCommand] =
        Command("rengbis")
            .withHelp(HelpDoc.p("ReNGBis - A content schema definition language for validating payloads."))
            .subcommands(validateSchemaCommand(exists), validateDataCommand(exists), translateSchemaFromCommand(exists), translateSchemaToCommand(exists))

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
            case RengbisCommand.ValidateSchema(schemaFiles)                                 => validateSchemas(schemaFiles)
            case RengbisCommand.ValidateData(format, schemaFile, schema, dataFiles)         => validateData(format, schemaFile, schema, dataFiles)
            case RengbisCommand.TranslateSchemaFrom(format, source, target, report)         => translateSchemaFrom(format, source, target, report)
            case RengbisCommand.TranslateSchemaTo(format, source, target, report, rootName) => translateSchemaTo(format, source, target, report, rootName)

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
            parser: DataParsers.Parser[Path] = format match
                                                   case Format.Json => DataParsers.json
                                                   case Format.Yaml => DataParsers.yaml
                                                   case Format.Xml  => DataParsers.xml
                                                   case Format.Csv  => DataParsers.csv
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

    def translateSchemaFrom(format: SchemaFormat, source: Path, target: Option[Path], reportPath: Option[Path]): ZIO[Any, Throwable, Unit] =
        import rengbis.translators.schemas.xsd.{ XsdImporter, XsdExporter }
        import rengbis.translators.schemas.jsonschema.{ JsonSchemaImporter, JsonSchemaExporter }
        import rengbis.translators.schemas.avro.{ AvroImporter, AvroExporter }
        import rengbis.translators.common.FrictionReport

        for
            sourceContent           <- ZIO.attempt(Files.readString(source))
            result                  <- ZIO.fromEither {
                                           format match
                                               case SchemaFormat.Xsd        => XsdImporter.fromXsd(sourceContent)
                                               case SchemaFormat.JsonSchema => JsonSchemaImporter.fromJsonSchema(sourceContent)
                                               case SchemaFormat.Avro       => AvroImporter.fromAvro(sourceContent)
                                       }.mapError(e => new RuntimeException(s"Failed to import schema: $e"))
            (schema, frictionReport) = result
            output                  <- ZIO.fromEither(SchemaSyntax.items.printString(schema).left.map(e => new RuntimeException(s"Failed to serialize schema: $e")))
            _                       <- target match
                                           case Some(path) => ZIO.attempt(Files.writeString(path, s"= $output"))
                                           case None       => Console.printLine(s"= $output")
            _                       <- reportPath match
                                           case Some(path) =>
                                               val reportContent = if path.toString.endsWith(".md") then frictionReport.toMarkdown else frictionReport.toPlainText
                                               ZIO.attempt(Files.writeString(path, reportContent)) *>
                                                   Console.printLine(s"Friction report written to ${ path.getFileName }")
                                           case None       => ZIO.unit
            _                       <- target match
                                           case Some(path) => Console.printLine(s"✓ Schema translated successfully to ${ path.getFileName }")
                                           case None       => ZIO.unit
        yield ()

    def translateSchemaTo(format: SchemaFormat, source: Path, target: Option[Path], reportPath: Option[Path], rootName: Option[String]): ZIO[Any, Throwable, Unit] =
        import rengbis.translators.schemas.xsd.{ XsdImporter, XsdExporter }
        import rengbis.translators.schemas.jsonschema.{ JsonSchemaImporter, JsonSchemaExporter }
        import rengbis.translators.schemas.avro.{ AvroImporter, AvroExporter }
        import rengbis.translators.common.FrictionReport
        import zio.json.EncoderOps

        for
            schema                     <- ZIO.fromEither(SchemaLoader.loadSchemaAtPath(source)).mapError(e => new RuntimeException(s"Failed to load ReNGBis schema: $e"))
            (outputRaw, frictionReport) = format match
                                              case SchemaFormat.Xsd        => XsdExporter.toXsd(schema.root.get, rootName.getOrElse("root"))
                                              case SchemaFormat.JsonSchema =>
                                                  val (json, report) = JsonSchemaExporter.toJsonSchema(schema.root.get)
                                                  (json.toJsonPretty, report)
                                              case SchemaFormat.Avro       =>
                                                  val (json, report) = AvroExporter.toAvro(schema.root.get, rootName.getOrElse("Record"))
                                                  (json.toJsonPretty, report)
            output                      = outputRaw
            _                          <- target match
                                              case Some(path) => ZIO.attempt(Files.writeString(path, output))
                                              case None       => Console.printLine(output)
            _                          <- reportPath match
                                              case Some(path) =>
                                                  val reportContent = if path.toString.endsWith(".md") then frictionReport.toMarkdown else frictionReport.toPlainText
                                                  ZIO.attempt(Files.writeString(path, reportContent)) *>
                                                      Console.printLine(s"Friction report written to ${ path.getFileName }")
                                              case None       => ZIO.unit
            _                          <- target match
                                              case Some(path) => Console.printLine(s"✓ Schema translated successfully to ${ path.getFileName }")
                                              case None       => ZIO.unit
        yield ()

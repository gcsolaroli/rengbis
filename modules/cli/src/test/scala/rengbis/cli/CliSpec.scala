package rengbis.cli

import zio.test.{ assertTrue, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses
import zio.ZIO
import zio.cli.{ CliConfig, CliError, CommandDirective }
import java.nio.file.{ Files, Paths }
import rengbis.cli.Main.{ Format, RengbisCommand }
import zio.test.{ assertZIO, TestConstructor, TestResult }
import zio.test.Assertion.equalTo
import zio.internal.stacktracer.{ SourceLocation, Tracer }
import zio.cli.Exists

object CliSpec extends ZIOSpecDefault:
    def spec = suite("CLI command parsing")(
        testCommand(
            "rengbis validate-schema /tmp/schema.rengbis",
            validateSchema("/tmp/schema.rengbis")
        ),
        testCommand(
            "rengbis validate-schema /tmp/schema1.rengbis /tmp/schema2.rengbis",
            validateSchema("/tmp/schema1.rengbis", "/tmp/schema2.rengbis")
        ),
        testCommand(
            "rengbis validate-data --format json --schema /tmp/schema.rengbis /tmp/data.json",
            validateData(format = "json", schema = "/tmp/schema.rengbis", files = "/tmp/data.json")
        ),
        testCommand(
            "rengbis validate-data --format yaml --schema /tmp/schema.rengbis /tmp/data.yaml",
            validateData(format = "yaml", schema = "/tmp/schema.rengbis", files = "/tmp/data.yaml")
        ),
        testCommand(
            "rengbis validate-data --format xml --schema /tmp/schema.rengbis /tmp/data.xml",
            validateData(format = "xml", schema = "/tmp/schema.rengbis", files = "/tmp/data.xml")
        ),
        testCommand(
            "rengbis validate-data -f json -s /tmp/schema.rengbis /tmp/data.json",
            validateData(format = "json", schema = "/tmp/schema.rengbis", files = "/tmp/data.json")
        ),
        testCommand(
            "rengbis validate-data -f json -s /tmp/schema.rengbis /tmp/data1.json /tmp/data2.json /tmp/data3.json",
            validateData(format = "json", schema = "/tmp/schema.rengbis", files = "/tmp/data1.json", "/tmp/data2.json", "/tmp/data3.json")
        ),
        testCommand(
            "rengbis validate-data -s /tmp/schema.rengbis -f yaml /tmp/data.yaml",
            validateData(format = "yaml", schema = "/tmp/schema.rengbis", files = "/tmp/data.yaml")
        )
    )

    def validateSchema(schemas: String*): RengbisCommand =
        RengbisCommand.ValidateSchema(schemas.toList.map(s => Paths.get(s)))

    def validateData(schema: String, format: String, files: String*): RengbisCommand =
        val formatValue: Format = format match
            case "json" => Format.Json
            case "xml"  => Format.Xml
            case "yaml" => Format.Yaml

        RengbisCommand.ValidateData(format = formatValue, schemaFile = Paths.get(schema), dataFiles = files.toList.map(s => Paths.get(s)))

    def testCommand[In](command: String, expectedCommand: RengbisCommand) =
        test(command):
            val args          = command.split("\\s+").toList
            val parsedCommand = Main
                .buildCommand(Exists.No)
                .parse(args, CliConfig.default)
                .flatMap(directive =>
                    directive match
                        case CommandDirective.UserDefined(_, value) => ZIO.succeed(value)
                        case _                                      => ZIO.fail(Exception.apply("PIPPO"))
                )
            assertZIO(parsedCommand)(equalTo(expectedCommand))

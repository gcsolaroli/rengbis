package rengbis

import zio.{ Task, ZIO }
import zio.test.{ assertTrue, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses

import java.nio.file.{ Files, Path, Paths }
import scala.jdk.CollectionConverters.*

import rengbis.Schema.*

object SchemaSpec extends ZIOSpecDefault:
    def parseTest(text: String, expectedValue: Schema) = test(text) { assertTrue(parse(text) == Right(expectedValue)) }

    def spec = suite("Schema parsing features")(
        parseTest("= number", NumericValue()),
        parseTest("= text", TextValue()),
        parseTest("= number*", ListOfValues(NumericValue())),
        parseTest("= number+", ListOfValues(NumericValue(), ListConstraint.MinSize(1))),
        parseTest("= text | number", AlternativeValues(TextValue(), NumericValue())),
        parseTest("= text | number*", AlternativeValues(TextValue(), ListOfValues(NumericValue()))),
        parseTest("= text* | number", AlternativeValues(ListOfValues(TextValue()), NumericValue())),
        parseTest("= (text | number)*", ListOfValues(AlternativeValues(TextValue(), NumericValue()))),
        parseTest("""= "yes" | "no" """, EnumValues("yes", "no")),
        parseTest("""= text { length == 10 }""", TextValue(TextConstraint.Length(10))),
        parseTest("""= text { 10 <= length <= 100 }""", TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100))),
        parseTest("""= text { 10 < length < 100 }""", TextValue(TextConstraint.MinLength(11), TextConstraint.MaxLength(99))),
        parseTest("""= text { 10 < length <= 100 }""", TextValue(TextConstraint.MinLength(11), TextConstraint.MaxLength(100))),
        parseTest("""= text { 10 <= length < 100 }""", TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(99))),
        parseTest("""= text { 10 <= length }""", TextValue(TextConstraint.MinLength(10))),
        parseTest("""= text { 10 < length }""", TextValue(TextConstraint.MinLength(11))),
        parseTest("""= text { length >= 10 }""", TextValue(TextConstraint.MinLength(10))),
        parseTest("""= text { length > 10 }""", TextValue(TextConstraint.MinLength(11))),
        parseTest("""= text { length <= 100 }""", TextValue(TextConstraint.MaxLength(100))),
        parseTest("""= text { length < 100 }""", TextValue(TextConstraint.MaxLength(99))),
        parseTest("""= number { integer }""", NumericValue(NumericConstraint.Integer)),
        parseTest("""= number { value >= 0 }""", NumericValue(NumericConstraint.MinValue(0))),
        parseTest("""= number { value > 0 }""", NumericValue(NumericConstraint.MinValueExclusive(0))),
        parseTest("""= number { value <= 100 }""", NumericValue(NumericConstraint.MaxValue(100))),
        parseTest("""= number { value < 100 }""", NumericValue(NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number { value == 42 }""", NumericValue(NumericConstraint.ExactValue(42))),
        parseTest("""= number { 0 <= value <= 100 }""", NumericValue(NumericConstraint.MinValue(0), NumericConstraint.MaxValue(100))),
        parseTest("""= number { 0 < value < 100 }""", NumericValue(NumericConstraint.MinValueExclusive(0), NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number { 0 < value <= 100 }""", NumericValue(NumericConstraint.MinValueExclusive(0), NumericConstraint.MaxValue(100))),
        parseTest("""= number { 0 <= value < 100 }""", NumericValue(NumericConstraint.MinValue(0), NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number { integer, value >= 0 }""", NumericValue(NumericConstraint.Integer, NumericConstraint.MinValue(0))),
        parseTest("""= number { integer, 1 <= value <= 12 }""", NumericValue(NumericConstraint.Integer, NumericConstraint.MinValue(1), NumericConstraint.MaxValue(12))),
        parseTest("""= number { value >= -10 }""", NumericValue(NumericConstraint.MinValue(-10))),
        parseTest("""= number { value >= 0.5 }""", NumericValue(NumericConstraint.MinValue(BigDecimal("0.5")))),
        parseTest("""= text { 10 <= length <= 100 }*""", ListOfValues(TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)))),
        parseTest("""= text{10}""", ListOfValues(TextValue(), ListConstraint.ExactSize(10))),
        parseTest(
            """= text { 10 <= length <= 100 }{10}""",
            ListOfValues(TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)), ListConstraint.ExactSize(10))
        ),

        // ----------------------------------------------------------------
        test("with extra empty lines"):
            val schemaDefinition = """

= text | number


"""
            assertTrue(parse(schemaDefinition) == Right(AlternativeValues(TextValue(), NumericValue())))
        , // ----------------------------------------------------------------
        test("simple JSON/Yaml structure"):
            val schemaDefinition = """= {
	name: text,
	age: number,
	hobbies: text*
}"""
            val expectedSchema   = ObjectValue(
                Map(
                    MandatoryLabel("name")    -> TextValue(),
                    MandatoryLabel("age")     -> NumericValue(),
                    MandatoryLabel("hobbies") -> ListOfValues(TextValue())
                )
            )

            assertTrue(parse(schemaDefinition) == Right(expectedSchema))
        , // ----------------------------------------------------------------
        test("named value"):
            val schemaDefinition = """
foo = number*
= foo
"""
            assertTrue(parse(schemaDefinition) == Right(ListOfValues(NumericValue())))
        , // ----------------------------------------------------------------
        test("named values"):
            val schemaDefinition = """
foo = number*
bar = text*
= foo | bar
"""
            assertTrue(parse(schemaDefinition) == Right(AlternativeValues(ListOfValues(NumericValue()), ListOfValues(TextValue()))))
        , // ----------------------------------------------------------------
        test("named values inside object"):
            val schemaDefinition = """
foo = number*
bar = text*
= {
    foo: foo,
    bar: bar
}
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    ObjectValue(
                        Map(
                            (MandatoryLabel("foo") -> ListOfValues(NumericValue())),
                            (MandatoryLabel("bar") -> ListOfValues(TextValue()))
                        )
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("nested named values"):
            val schemaDefinition = """
foo = number*
bar = text* | foo

= bar
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    AlternativeValues(
                        ListOfValues(TextValue()),
                        ListOfValues(NumericValue())
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("named value object"):
            val schemaDefinition = """
bar = {
    bar_key_1: text*,
    bar_key_2: number*
}

= {
    key_1: number,
    key_2: bar
}
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    ObjectValue(
                        Map(
                            (MandatoryLabel("key_1") -> NumericValue()),
                            (MandatoryLabel("key_2") -> ObjectValue(
                                Map(
                                    (MandatoryLabel("bar_key_1") -> ListOfValues(TextValue())),
                                    (MandatoryLabel("bar_key_2") -> ListOfValues(NumericValue()))
                                )
                            ))
                        )
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("nested named value object"):
            val schemaDefinition = """
foo = {
    foo_key_1: number,
    foo_key_2: text
}

bar = {
    bar_key_1: text*,
    bar_key_2: foo*
}

= {
    key_1: number,
    key_2: bar
}
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    ObjectValue(
                        Map(
                            (MandatoryLabel("key_1") -> NumericValue()),
                            (MandatoryLabel("key_2") -> ObjectValue(
                                Map(
                                    (MandatoryLabel("bar_key_1") -> ListOfValues(TextValue())),
                                    (MandatoryLabel("bar_key_2") -> ListOfValues(
                                        ObjectValue(
                                            (Map(
                                                (MandatoryLabel("foo_key_1") -> NumericValue()),
                                                (MandatoryLabel("foo_key_2") -> TextValue())
                                            ))
                                        )
                                    ))
                                )
                            ))
                        )
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("simple tuple value"):
            val schemaDefinition = """
= (text, text, number)
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    TupleValue(
                        TextValue(),
                        TextValue(),
                        NumericValue()
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("tuple value"):
            val schemaDefinition = """
= (text { 10 <= length <= 100 }, text { length <= 10 }, number)
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    TupleValue(
                        TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)),
                        TextValue(TextConstraint.MaxLength(10)),
                        NumericValue()
                    )
                )
            )
        // ----------------------------------------------------------------
    )

// ############################################################################
/*
object SchemaFailSpec extends ZIOSpecDefault:
    def spec = suite("Schema parsing errors")(
        test("degenerated tuple value"):
            val schemaDefinition = """
= (text)
"""
            allSuccesses(
                assertTrue(parse(schemaDefinition).isLeft),
                assertTrue(parse(schemaDefinition).swap.getOrElse("").contains("tuple needs to have at least two items"))
            )
    )
 */
// ############################################################################

object SchemaSamplesSpec extends ZIOSpecDefault:
    type FileValidationResult = (String, Either[String, String])

    def isExpectedToFail(path: Path): Boolean =
        val fileName       = path.getFileName.toString
        val nameWithoutExt = fileName.lastIndexOf('.') match
            case -1 => fileName
            case i  => fileName.substring(0, i)
        nameWithoutExt.endsWith("-NOT_VALID")

    def validateSchemaFiles(schemasDir: Path): Task[List[FileValidationResult]] =
        ZIO.attempt:
            Files
                .list(schemasDir)
                .iterator()
                .asScala
                .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".rengbis"))
                .map { schemaFile =>
                    val fileName       = schemaFile.getFileName.toString
                    val content        = Files.readString(schemaFile)
                    val parseResult    = Schema.parse(content)
                    val expectedToFail = isExpectedToFail(schemaFile)

                    (parseResult, expectedToFail) match
                        case (Right(_), false)  => (fileName, Right("valid as expected"))
                        case (Left(_), true)    => (fileName, Right("invalid as expected"))
                        case (Right(_), true)   => (fileName, Left("expected to be invalid but parsed successfully"))
                        case (Left(err), false) => (fileName, Left(s"expected to be valid but failed: $err"))
                }
                .toList

    def validateDataFiles(schemasDir: Path): Task[List[FileValidationResult]] =
        ZIO.attempt:
            val schemaDirs = Files
                .list(schemasDir)
                .iterator()
                .asScala
                .filter(p => Files.isDirectory(p))
                .filter(p => Files.exists(p.resolveSibling(p.getFileName.toString + ".rengbis")))
                .toList

            schemaDirs.flatMap { schemaDir =>
                val schemaName = schemaDir.getFileName.toString
                val schemaFile = schemasDir.resolve(s"$schemaName.rengbis")

                if !Files.exists(schemaFile) then List((s"$schemaName/", Left(s"no matching schema file found: $schemaName.rengbis")))
                else
                    val schemaContent = Files.readString(schemaFile)
                    Schema.parse(schemaContent) match
                        case Left(err)     =>
                            List((schemaFile.getFileName.toString, Left(s"schema parse error: $err")))
                        case Right(schema) =>
                            validateDataFilesForSchema(schemaDir, schema)
            }

    def validateDataFilesForSchema(schemaDir: Path, schema: Schema): List[FileValidationResult] =
        val formatDirs = Files
            .list(schemaDir)
            .iterator()
            .asScala
            .filter(p => Files.isDirectory(p))
            .toList

        formatDirs.flatMap { formatDir =>
            val formatName                                      = formatDir.getFileName.toString.toLowerCase
            val parser: Option[String => Either[String, Value]] = formatName match
                case "json" => Some(DataParsers.json)
                case "yaml" => Some(DataParsers.yaml)
                case "xml"  => Some(DataParsers.xml)
                case _      => None

            parser match
                case None             =>
                    List((s"${ schemaDir.getFileName }/$formatName/", Left(s"unknown format: $formatName (expected json, yaml, or xml)")))
                case Some(dataParser) =>
                    val dataFiles = Files
                        .list(formatDir)
                        .iterator()
                        .asScala
                        .filter(p => Files.isRegularFile(p))
                        .toList

                    dataFiles.map { dataFile =>
                        val content        = Files.readString(dataFile)
                        val result         = Validator.validateString(dataParser)(schema, content)
                        val expectedToFail = isExpectedToFail(dataFile)
                        val relativePath   = s"${ schemaDir.getFileName }/$formatName/${ dataFile.getFileName }"

                        (result.isValid, expectedToFail) match
                            case (true, false)  => (relativePath, Right("valid as expected"))
                            case (false, true)  => (relativePath, Right("invalid as expected"))
                            case (true, true)   => (relativePath, Left("expected to be invalid but validated successfully"))
                            case (false, false) => (relativePath, Left(s"expected to be valid but failed: ${ result.errorMessage }"))
                    }
        }

    def spec = suite("Schema parsing samples")(
        test("validate all schema and data files in resources/schemas"):
            val schemasDir = Paths.get(getClass.getClassLoader.getResource("schemas").toURI)

            for
                schemaResults <- validateSchemaFiles(schemasDir)
                dataResults   <- validateDataFiles(schemasDir)
                allResults     = schemaResults ++ dataResults
                failures       = allResults.collect { case (file, Left(error)) => s"  - $file: $error" }
            yield
                if failures.isEmpty then assertTrue(true)
                else assertTrue(false) ?? s"Validation failures:\n${ failures.mkString("\n") }"
    )

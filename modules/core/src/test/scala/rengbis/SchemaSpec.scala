package rengbis

import zio.test.{ assertTrue, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses

import java.nio.file.{ Files, Path, Paths }
import scala.jdk.CollectionConverters.IteratorHasAsScala
import rengbis.Schema.{ AlternativeValues, AnyValue, EnumValues, ListOfValues, MandatoryLabel, NumericValue, ObjectValue, Schema, TextValue, TupleValue }
import rengbis.Schema.{ ListConstraint, NumericConstraint, TextConstraint }

def parse(text: String): Either[String, Schema] = SchemaLoader.parseSchema(text).map(s => s.root.get)

object SchemaSpec extends ZIOSpecDefault:
    def parseTest(text: String, expectedValue: Schema) = test(text) { assertTrue(parse(text) == Right(expectedValue)) }

    def spec = suite("Schema parsing features")(
        parseTest("= any", AnyValue()),
        parseTest("= number", NumericValue()),
        parseTest("= text", TextValue()),
        parseTest("= number*", ListOfValues(NumericValue())),
        parseTest("= number+", ListOfValues(NumericValue(), ListConstraint.MinSize(1))),
        parseTest("= text | number", AlternativeValues(TextValue(), NumericValue())),
        parseTest("= text | number*", AlternativeValues(TextValue(), ListOfValues(NumericValue()))),
        parseTest("= text* | number", AlternativeValues(ListOfValues(TextValue()), NumericValue())),
        parseTest("= (text | number)*", ListOfValues(AlternativeValues(TextValue(), NumericValue()))),
        parseTest("""= "yes" | "no" """, EnumValues("yes", "no")),
        parseTest("""= text [ length == 10 ]""", TextValue(TextConstraint.Length(10))),
        parseTest("""= text [ 10 <= length <= 100 ]""", TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100))),
        parseTest("""= text [ 10 < length < 100 ]""", TextValue(TextConstraint.MinLength(11), TextConstraint.MaxLength(99))),
        parseTest("""= text [ 10 < length <= 100 ]""", TextValue(TextConstraint.MinLength(11), TextConstraint.MaxLength(100))),
        parseTest("""= text [ 10 <= length < 100 ]""", TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(99))),
        parseTest("""= text [ 10 <= length ]""", TextValue(TextConstraint.MinLength(10))),
        parseTest("""= text [ 10 < length ]""", TextValue(TextConstraint.MinLength(11))),
        parseTest("""= text [ length >= 10 ]""", TextValue(TextConstraint.MinLength(10))),
        parseTest("""= text [ length > 10 ]""", TextValue(TextConstraint.MinLength(11))),
        parseTest("""= text [ length <= 100 ]""", TextValue(TextConstraint.MaxLength(100))),
        parseTest("""= text [ length < 100 ]""", TextValue(TextConstraint.MaxLength(99))),
        parseTest("""= number [ integer ]""", NumericValue(NumericConstraint.Integer)),
        parseTest("""= number [ value >= 0 ]""", NumericValue(NumericConstraint.MinValue(0))),
        parseTest("""= number [ value > 0 ]""", NumericValue(NumericConstraint.MinValueExclusive(0))),
        parseTest("""= number [ value <= 100 ]""", NumericValue(NumericConstraint.MaxValue(100))),
        parseTest("""= number [ value < 100 ]""", NumericValue(NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number [ value == 42 ]""", NumericValue(NumericConstraint.ExactValue(42))),
        parseTest("""= number [ 0 <= value <= 100 ]""", NumericValue(NumericConstraint.MinValue(0), NumericConstraint.MaxValue(100))),
        parseTest("""= number [ 0 < value < 100 ]""", NumericValue(NumericConstraint.MinValueExclusive(0), NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number [ 0 < value <= 100 ]""", NumericValue(NumericConstraint.MinValueExclusive(0), NumericConstraint.MaxValue(100))),
        parseTest("""= number [ 0 <= value < 100 ]""", NumericValue(NumericConstraint.MinValue(0), NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number [ integer, value >= 0 ]""", NumericValue(NumericConstraint.Integer, NumericConstraint.MinValue(0))),
        parseTest("""= number [ integer, 1 <= value <= 12 ]""", NumericValue(NumericConstraint.Integer, NumericConstraint.MinValue(1), NumericConstraint.MaxValue(12))),
        parseTest("""= number [ value >= -10 ]""", NumericValue(NumericConstraint.MinValue(-10))),
        parseTest("""= number [ value >= 0.5 ]""", NumericValue(NumericConstraint.MinValue(BigDecimal("0.5")))),
        parseTest("""= text [ 10 <= length <= 100 ]*""", ListOfValues(TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)))),
        parseTest("""= text* [ size == 10 ]""", ListOfValues(TextValue(), ListConstraint.ExactSize(10))),
        parseTest(
            """= text [ 10 <= length <= 100 ]* [ size == 10 ]""",
            ListOfValues(TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)), ListConstraint.ExactSize(10))
        ),
        parseTest("""= text* [ unique ]""", ListOfValues(TextValue(), ListConstraint.Unique)),
        parseTest("""= number+ [ unique ]""", ListOfValues(NumericValue(), ListConstraint.MinSize(1), ListConstraint.Unique)),
        parseTest("""= text* [ unique, size == 3 ]""", ListOfValues(TextValue(), ListConstraint.Unique, ListConstraint.ExactSize(3))),
        parseTest("""= text* [ unique, 2 <= size <= 5 ]""", ListOfValues(TextValue(), ListConstraint.Unique, ListConstraint.MinSize(2), ListConstraint.MaxSize(5))),
        parseTest(
            """= { id: text }* [ unique = id ]""",
            ListOfValues(ObjectValue(Map(MandatoryLabel("id") -> TextValue())), ListConstraint.UniqueByFields(Seq("id")))
        ),
        parseTest(
            """= { id: text, name: text }* [ unique = (id, name) ]""",
            ListOfValues(
                ObjectValue(Map(MandatoryLabel("id") -> TextValue(), MandatoryLabel("name") -> TextValue())),
                ListConstraint.UniqueByFields(Seq("id", "name"))
            )
        ),
        parseTest(
            """= { id: text, code: text }* [ unique = id, unique = code ]""",
            ListOfValues(
                ObjectValue(Map(MandatoryLabel("id") -> TextValue(), MandatoryLabel("code") -> TextValue())),
                ListConstraint.UniqueByFields(Seq("id")),
                ListConstraint.UniqueByFields(Seq("code"))
            )
        ),
        parseTest("""= text* [ size == 5 ]""", ListOfValues(TextValue(), ListConstraint.ExactSize(5))),
        parseTest("""= text* [ size >= 2 ]""", ListOfValues(TextValue(), ListConstraint.MinSize(2))),
        parseTest("""= text* [ size > 2 ]""", ListOfValues(TextValue(), ListConstraint.MinSize(3))),
        parseTest("""= text* [ size <= 10 ]""", ListOfValues(TextValue(), ListConstraint.MaxSize(10))),
        parseTest("""= text* [ size < 10 ]""", ListOfValues(TextValue(), ListConstraint.MaxSize(9))),
        parseTest("""= text* [ 2 <= size <= 5 ]""", ListOfValues(TextValue(), ListConstraint.MinSize(2), ListConstraint.MaxSize(5))),
        parseTest("""= text* [ 2 < size < 10 ]""", ListOfValues(TextValue(), ListConstraint.MinSize(3), ListConstraint.MaxSize(9))),
        parseTest("""= text* [ 2 <= size ]""", ListOfValues(TextValue(), ListConstraint.MinSize(2))),
        parseTest(
            """= text* [ unique, 2 <= size <= 5 ]""",
            ListOfValues(TextValue(), ListConstraint.Unique, ListConstraint.MinSize(2), ListConstraint.MaxSize(5))
        ), // ----------------------------------------------------------------
        test("with extra empty lines"):
            val schemaDefinition = """

= text | number


"""
            assertTrue(parse(schemaDefinition) == Right(AlternativeValues(TextValue(), NumericValue())))
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
        test("named value"):
            val schemaDefinition = """
foo = number*
= foo
"""
            assertTrue(parse(schemaDefinition) == Right(ListOfValues(NumericValue())))
        ,  // ----------------------------------------------------------------
        test("named values"):
            val schemaDefinition = """
foo = number*
bar = text*
= foo | bar
"""
            assertTrue(parse(schemaDefinition) == Right(AlternativeValues(ListOfValues(NumericValue()), ListOfValues(TextValue()))))
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
        test("tuple value"):
            val schemaDefinition = """
= (text [ 10 <= length <= 100 ], text [ length <= 10 ], number)
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
    import zio.test.Spec

    def isExpectedToFail(path: Path): Boolean =
        val fileName       = path.getFileName.toString
        val nameWithoutExt = fileName.lastIndexOf('.') match
            case -1 => fileName
            case i  => fileName.substring(0, i)
        nameWithoutExt.endsWith("-NOT_VALID")

    def schemaFileTest(schemaFile: Path): Spec[Any, Nothing] =
        val fileName       = schemaFile.getFileName.toString
        val content        = Files.readString(schemaFile)
        val parseResult    = parse(content)
        val expectedToFail = isExpectedToFail(schemaFile)

        test(fileName):
            (parseResult, expectedToFail) match
                case (Right(_), false)  => assertTrue(true) ?? "valid as expected"
                case (Left(_), true)    => assertTrue(true) ?? "invalid as expected"
                case (Right(_), true)   => assertTrue(false) ?? "expected to be invalid but parsed successfully"
                case (Left(err), false) => assertTrue(false) ?? s"expected to be valid but failed: $err"

    def getDataFilesForSchema(schemaDir: Path): List[(Path, DataParsers.Parser[Path])] =
        val formatDirs = Files
            .list(schemaDir)
            .iterator()
            .asScala
            .filter(p => Files.isDirectory(p))
            .toList

        formatDirs.flatMap { formatDir =>
            val formatName                               = formatDir.getFileName.toString.toLowerCase
            val parser: Option[DataParsers.Parser[Path]] = formatName match
                case "json" => Some(DataParsers.json)
                case "yaml" => Some(DataParsers.yaml)
                case "xml"  => Some(DataParsers.xml)
                case "csv"  => Some(DataParsers.csv)
                case _      => None

            parser match
                case None         => Nil
                case Some(parser) =>
                    Files
                        .list(formatDir)
                        .iterator()
                        .asScala
                        .filter(p => Files.isRegularFile(p))
                        .map(p => (p, parser))
                        .toList
        }

    def dataFileTest(schemaDir: Path, schema: Schema, dataFile: Path, parser: DataParsers.Parser[Path]): Spec[Any, Nothing] =
        // val content        = Files.readString(dataFile)
        val result         = Validator.validate(parser(dataFile))(schema)
        val expectedToFail = isExpectedToFail(dataFile)
        val formatName     = dataFile.getParent.getFileName.toString
        val relativePath   = s"$formatName/${ dataFile.getFileName }"

        test(relativePath):
            (result.isValid, expectedToFail) match
                case (true, false)  => assertTrue(true) ?? "valid as expected"
                case (false, true)  => assertTrue(true) ?? "invalid as expected"
                case (true, true)   => assertTrue(false) ?? "expected to be invalid but validated successfully"
                case (false, false) => assertTrue(false) ?? s"expected to be valid but failed: ${ result.errorMessage }"

    def dataSuiteForSchema(schemasDir: Path, schemaDir: Path): Spec[Any, Nothing] =
        val schemaName   = schemaDir.getFileName.toString
        val schemaFile   = schemasDir.resolve(s"$schemaName.rengbis")
        // val schemaContent = Files.readString(schemaFile)
        val loadedSchema = SchemaLoader.loadSchemaAtPath(schemaFile).flatMap(_.getRootSchema)

        loadedSchema match
            case Left(err)     =>
                suite(schemaName)(test("schema parsing")(assertTrue(false) ?? s"schema parse error: ${ err }"))
            case Right(schema) =>
                val dataFiles = getDataFilesForSchema(schemaDir).sortBy(_._1.getFileName.toString)
                val tests     = dataFiles.map { case (dataFile, parser) => dataFileTest(schemaDir, schema, dataFile, parser) }
                suite(schemaName)(tests*)

    def getSchemaFiles(schemasDir: Path): List[Path] =
        Files
            .list(schemasDir)
            .iterator()
            .asScala
            .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".rengbis"))
            .filter(p => testFilter(p.getFileName.toString))
            .toList
            .sortBy(_.getFileName.toString)

    def getSchemaDirs(schemasDir: Path): List[Path] =
        Files
            .list(schemasDir)
            .iterator()
            .asScala
            .filter(p => Files.isDirectory(p))
            .filter(p => Files.exists(p.resolveSibling(p.getFileName.toString + ".rengbis")))
            .filter(p => testFilter(p.getFileName.toString))
            .toList
            .sortBy(_.getFileName.toString)

    def testFilter(filename: String): Boolean =
        val lastDot  = filename.lastIndexOf('.')
        val testName = if lastDot > 0 then filename.substring(0, lastDot) else filename

        true
        // testName == "simple-recursive"

    def spec =
        val schemasDir = Paths.get(getClass.getClassLoader.getResource("schemas").toURI)

        val schemaFileTests = getSchemaFiles(schemasDir).map(schemaFileTest)
        val dataSuites      = getSchemaDirs(schemasDir).map(dir => dataSuiteForSchema(schemasDir, dir))

        suite("Schema samples")(
            suite("Schema file parsing")(schemaFileTests*),
            suite("Data file validation")(dataSuites*)
        )

package rengbis

import zio.test.{ assertTrue, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses

import java.nio.file.{ Files, Path, Paths }
import scala.jdk.CollectionConverters.IteratorHasAsScala
import rengbis.Schema.{ AlternativeValues, AnyValue, BinaryValue, EnumValues, ListOfValues, MandatoryLabel, NumericValue, ObjectValue, Schema, TextValue, TupleValue }
import rengbis.Schema.{ BinaryConstraint, ListConstraint, NumericConstraint, TextConstraint }

//
//  This class is taking care of running a set of test driven by the content of the `./modules/core/src/test/resouces/schemas` folder.
//  More information on how the content of that folder is used to run these tests is available here: `./modules/core/src/test/resources/schemas/README.md`
//
object SchemaSamplesSpec extends ZIOSpecDefault:
    import zio.test.Spec

    def testFilter(filename: String): Boolean =
        val lastDot  = filename.lastIndexOf('.')
        val testName = if lastDot > 0 then filename.substring(0, lastDot) else filename

        true
        // testName == "simple-recursive"   //  use to focus on some failing tests, to remove some noise.

    def isExpectedToFail(path: Path): Boolean =
        val fileName       = path.getFileName.toString
        val nameWithoutExt = fileName.lastIndexOf('.') match
            case -1 => fileName
            case i  => fileName.substring(0, i)
        nameWithoutExt.endsWith("-NOT_VALID")

    def schemaFileTest(schemaFile: Path): Spec[Any, Nothing] =
        // val fileName       = schemaFile.getFileName.toString
        // val content        = Files.readString(schemaFile)
        // val parseResult    = parse(content)
        val schema         = SchemaLoader.loadSchemaAtPath(schemaFile)
        val expectedToFail = isExpectedToFail(schemaFile)

        test(schemaFile.getFileName.toString):
            (schema, expectedToFail) match
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

    def spec =
        val schemasDir = Paths.get(getClass.getClassLoader.getResource("schemas").toURI)

        val schemaFileTests = getSchemaFiles(schemasDir).map(schemaFileTest)
        val dataSuites      = getSchemaDirs(schemasDir).map(dir => dataSuiteForSchema(schemasDir, dir))

        suite("Schema samples")(
            suite("Schema file parsing")(schemaFileTests*),
            suite("Data file validation")(dataSuites*)
        )

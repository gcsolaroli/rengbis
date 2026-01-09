package rengbis

import zio.{ Task, ZIO }
import zio.test.{ assertTrue, ZIOSpecDefault }
import java.nio.file.{ Files, Path, Paths }
import scala.jdk.CollectionConverters.*
import rengbis.Schema.Schema

object SchemaLoaderSpec extends ZIOSpecDefault:

    type FileValidationResult = (String, Either[String, String])

    def validateFile(structuredDir: Path)(expectedFile: Path): (String, Either[String, String]) =
        val fileName       = expectedFile.getFileName.toString
        val structuredFile = structuredDir.resolve(fileName)

        if !Files.exists(structuredFile) then (fileName, Left(s"matching structured file not found: $structuredFile"))
        else
            val expectedSchema = SchemaLoader.loadSchemaAtPath(expectedFile)
            val resolvedSchema = SchemaLoader.loadSchemaAtPath(structuredFile)

            (expectedSchema, resolvedSchema) match
                case (Left(err), _)           => (fileName, Left(s"failed to parse expected file: $err"))
                case (_, Left(err))           => (fileName, Left(s"failed to resolve structured file: $err"))
                case (Right(exp), Right(res)) =>
                    if exp.root == res.root then (fileName, Right("resolved schema matches expected"))
                    else (fileName, Left(s"schema mismatch:\n  expected: ${ exp.toString() }\n  resolved: ${ res.toString() }"))

    def validateLoaderFiles(loaderDir: Path): Task[List[FileValidationResult]] =
        ZIO.attempt:
            val expectedDir   = loaderDir.resolve("expected")
            val structuredDir = loaderDir.resolve("structured")

            Files
                .list(expectedDir)
                .iterator()
                .asScala
                .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".rengbis"))
                .map(validateFile(structuredDir))
                .toList

    def spec = suite("SchemaLoader")(
        test("validate all loader test files in resources/schemas/loader"):
            val loaderDir = Paths.get(getClass.getClassLoader.getResource("schemas/loader").toURI)

            for
                results <- validateLoaderFiles(loaderDir)
                failures = results.collect { case (file, Left(error)) => s"  - $file: $error" }
            yield
                if failures.isEmpty then assertTrue(true)
                else assertTrue(false) ?? s"Loader validation failures:\n${ failures.mkString("\n") }"
    )

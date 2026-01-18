package rengbis.translators.schemas

import rengbis.Schema.Schema
import rengbis.SchemaSyntax
import rengbis.translators.schemas.jsonschema.JsonSchemaImporter
import rengbis.translators.schemas.xsd.XsdImporter
import zio.*
import zio.test.*

import scala.io.Source
import scala.util.Using

object SampleSchemasSpec extends ZIOSpecDefault:

    val basePath = "modules/translators/src/test/resources/sampleSchemas"

    def printSchema(schema: Schema): String =
        SchemaSyntax.items.printString(schema) match
            case Left(err)  => s"<print error: $err>"
            case Right(str) => str

    def testJsonSchemaImport(name: String) = test(s"imports $name"):
        val path    = s"$basePath/JsonSchema/$name"
        val content = Using(Source.fromFile(path))(_.mkString).get

        JsonSchemaImporter.fromJsonSchema(content) match
            case Left(error)               =>
                println(s"ERROR importing $name: $error")
                assertTrue(false)
            case Right((schema, friction)) =>
                println(s"\n=== $name ===")
                println(s"Friction: ${ friction.entries.size } issues")
                friction.entries.take(5).foreach(e => println(s"  - [${ e.path }] ${ e.message }"))
                if friction.entries.size > 5 then println(s"  ... and ${ friction.entries.size - 5 } more")
                val printed = printSchema(schema)
                println(s"Schema (${ printed.length } chars):")
                println(printed.take(300))
                if printed.length > 300 then println("...")
                assertTrue(true)

    def testXsdImport(name: String) = test(s"imports $name"):
        val path    = s"$basePath/xsd/$name"
        val content = Using(Source.fromFile(path))(_.mkString).get

        XsdImporter.fromXsd(content) match
            case Left(error)               =>
                println(s"ERROR importing $name: $error")
                assertTrue(false)
            case Right((schema, friction)) =>
                println(s"\n=== $name ===")
                println(s"Friction: ${ friction.entries.size } issues")
                friction.entries.take(5).foreach(e => println(s"  - [${ e.path }] ${ e.message }"))
                if friction.entries.size > 5 then println(s"  ... and ${ friction.entries.size - 5 } more")
                val printed = printSchema(schema)
                println(s"Schema (${ printed.length } chars):")
                println(printed.take(300))
                if printed.length > 300 then println("...")
                assertTrue(true)

    override def spec = suite("Sample Schemas Translation")(
        suite("JsonSchema imports")(
            testJsonSchemaImport("npm.json"),
            testJsonSchemaImport("eslintrc.json"),
            testJsonSchemaImport("tsconfig.json")
        ),
        suite("XSD imports")(
            testXsdImport("wsdl.xsd"),
            testXsdImport("rss-2.0.xsd"),
            testXsdImport("atom.xsd"),
            testXsdImport("svg.xsd")
        )
    )

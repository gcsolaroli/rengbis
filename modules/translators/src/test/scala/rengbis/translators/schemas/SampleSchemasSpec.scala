package rengbis.translators.schemas

import rengbis.Schema.Schema
import rengbis.Schema.*
import rengbis.SchemaSyntax
import rengbis.translators.schemas.jsonschema.JsonSchemaImporter
import rengbis.translators.schemas.xsd.XsdImporter
import rengbis.translators.common.FrictionType
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

    /** Recursively finds all schema types in the tree */
    def findSchemaTypes(schema: Schema, depth: Int = 0): Set[String] =
        val typeName              = schema.getClass.getSimpleName
        val children: Set[String] = schema match
            case s: ObjectValue         => s.obj.values.flatMap(findSchemaTypes(_, depth + 1)).toSet
            case s: ListOfValues        => findSchemaTypes(s.schema, depth + 1)
            case s: MapValue            => findSchemaTypes(s.valueSchema, depth + 1)
            case s: AlternativeValues   => s.options.flatMap(findSchemaTypes(_, depth + 1)).toSet
            case s: TupleValue          => s.options.flatMap(findSchemaTypes(_, depth + 1)).toSet
            case s: NamedValueReference => Set(s"NamedValueReference(${ s.reference })")
            case s: ScopedReference     => Set(s"ScopedReference(${ s.namespace }, ${ s.name })")
            case _                      => Set.empty
        children + typeName

    /** Find first unprintable schema by testing subschemas (depth-first) */
    def findFirstUnprintable(schema: Schema, path: String = "$"): Option[(String, Schema, String)] =
        // First check for empty collections which cause printer crashes
        schema match
            case s: AlternativeValues if s.options.isEmpty => return Some((path, schema, "empty AlternativeValues"))
            case s: TupleValue if s.options.isEmpty        => return Some((path, schema, "empty TupleValue"))
            case s: EnumValues if s.values.isEmpty         => return Some((path, schema, "empty EnumValues"))
            case _                                         => ()

        // Check children FIRST (depth-first search)
        val childResult: Option[(String, Schema, String)] = schema match
            case Documented(_, inner) => findFirstUnprintable(inner, path)
            case Deprecated(inner)    => findFirstUnprintable(inner, path)
            case s: ObjectValue       =>
                s.obj.toSeq.view.flatMap { case (label, child) =>
                    findFirstUnprintable(child, s"$path/$label")
                }.headOption
            case s: ListOfValues      => findFirstUnprintable(s.schema, s"$path/[]")
            case s: MapValue          => findFirstUnprintable(s.valueSchema, s"$path/{}")
            case s: AlternativeValues =>
                s.options.zipWithIndex.view.flatMap { case (opt, i) =>
                    findFirstUnprintable(opt, s"$path|$i")
                }.headOption
            case s: TupleValue        =>
                s.options.zipWithIndex.view.flatMap { case (opt, i) =>
                    findFirstUnprintable(opt, s"$path[$i]")
                }.headOption
            case _                    => None

        if childResult.isDefined then return childResult

        // Now try printing this schema
        try
            SchemaSyntax.items.printString(schema) match
                case Left(err) => Some((path, schema, err))
                case Right(_)  => None
        catch case e: Exception => Some((path, schema, s"Exception: ${ e.getMessage }"))

    def testJsonSchemaImportDetailed(name: String) = test(s"imports $name (detailed)"):
        val path    = s"$basePath/JsonSchema/$name"
        val content = Using(Source.fromFile(path))(_.mkString).get

        JsonSchemaImporter.fromJsonSchema(content) match
            case Left(error)               =>
                println(s"ERROR importing $name: $error")
                assertTrue(false)
            case Right((schema, friction)) =>
                // println(s"\n${ "=" * 70 }")
                // println(s"=== $name ===")
                // println(s"${ "=" * 70 }")

                // // Group friction by type
                // val byType = friction.entries.groupBy(_.frictionType)

                // println(s"\nTotal friction entries: ${ friction.entries.size }")

                // byType.foreach { case (frictionType, entries) =>
                //     println(s"\n--- ${ frictionType } (${ entries.size } issues) ---")
                //     // Group by message to see patterns
                //     val byMessage = entries.groupBy(_.message).toSeq.sortBy(-_._2.size)
                //     byMessage.foreach { case (msg, es) =>
                //         println(s"  [${ es.size }x] $msg")
                //         if es.size <= 3 then es.foreach(e => println(s"       at: ${ e.path.take(80) }..."))
                //     }
                // }

                // // First check for problematic schemas before printing
                // findFirstUnprintable(schema) match
                //     case Some((badPath, badSchema, err)) =>
                //         println(s"\nSchema print result: FAILED (pre-check)")
                //         println(s"  First unprintable at: $badPath")
                //         println(s"    Error: $err")
                //         println(s"    Schema type: ${ badSchema.getClass.getSimpleName }")
                //         println(s"    Schema: ${ badSchema.toString.take(200) }")
                //     case None                            =>
                //         val printed =
                //             try printSchema(schema)
                //             catch case e: Exception => s"<print exception: ${ e.getMessage }>"
                //         println(s"\nSchema print result: ${ if printed.startsWith("<print") then "FAILED" else s"OK (${ printed.length } chars)" }")
                //         if printed.startsWith("<print") then
                //             println(s"  Error: $printed")
                //             val types = findSchemaTypes(schema)
                //             println(s"  Schema types found: ${ types.size }")
                //             types.filter(_.startsWith("NamedValueReference")).foreach(t => println(s"    - $t"))
                //             types.filter(_.startsWith("ScopedReference")).foreach(t => println(s"    - $t"))

                assertTrue(true)

    def testJsonSchemaImport(name: String) = test(s"imports $name"):
        val path    = s"$basePath/JsonSchema/$name"
        val content = Using(Source.fromFile(path))(_.mkString).get

        JsonSchemaImporter.fromJsonSchema(content) match
            case Left(error)               =>
                println(s"ERROR importing $name: $error")
                assertTrue(false)
            case Right((schema, friction)) =>
                // println(s"\n=== $name ===")
                // println(s"Friction: ${ friction.entries.size } issues")
                // friction.entries.take(5).foreach(e => println(s"  - [${ e.path }] ${ e.message }"))
                // if friction.entries.size > 5 then println(s"  ... and ${ friction.entries.size - 5 } more")
                // val printed = printSchema(schema)
                // println(s"Schema (${ printed.length } chars):")
                // println(printed.take(300))
                // if printed.length > 300 then println("...")
                assertTrue(true)

    def testXsdImport(name: String) = test(s"imports $name"):
        val path    = s"$basePath/xsd/$name"
        val content = Using(Source.fromFile(path))(_.mkString).get

        XsdImporter.fromXsd(content) match
            case Left(error)               =>
                println(s"ERROR importing $name: $error")
                assertTrue(false)
            case Right((schema, friction)) =>
                // println(s"\n=== $name ===")
                // println(s"Friction: ${ friction.entries.size } issues")
                // friction.entries.take(5).foreach(e => println(s"  - [${ e.path }] ${ e.message }"))
                // if friction.entries.size > 5 then println(s"  ... and ${ friction.entries.size - 5 } more")
                // val printed = printSchema(schema)
                // println(s"Schema (${ printed.length } chars):")
                // println(printed.take(300))
                // if printed.length > 300 then println("...")
                assertTrue(true)

    def testSimplePrint = test("simple print test"):
        val ref      = NamedValueReference("stringOrStringArray")
        println(s"Reference: $ref")
        val refPrint = SchemaSyntax.items.printString(ref)
        println(s"Reference print: $refPrint")

        val obj      = ObjectValue(Map(OptionalLabel("test") -> NamedValueReference("rule")))
        println(s"Object: $obj")
        val objPrint = SchemaSyntax.items.printString(obj)
        println(s"Object print: $objPrint")

        // Test the reference names from eslintrc
        val ref2      = NamedValueReference("properties_ecmaFeatures")
        println(s"Reference2: $ref2")
        val ref2Print = SchemaSyntax.items.printString(ref2)
        println(s"Reference2 print: $ref2Print")

        // Test Documented wrapping ObjectValue
        val docObj      = Documented(Some("test doc"), ObjectValue(Map(OptionalLabel("x") -> TextValue())))
        println(s"DocObj: $docObj")
        val docObjPrint = SchemaSyntax.items.printString(docObj)
        println(s"DocObj print: $docObjPrint")

        // Test EnumValues with references (like in eslintrc)
        val enumRef      = EnumValues("a", "b")
        println(s"EnumRef: $enumRef")
        val enumRefPrint = SchemaSyntax.items.printString(enumRef)
        println(s"EnumRef print: $enumRefPrint")

        // Test Documented with EnumValues in ObjectValue
        val docObjWithEnum      = Documented(
            Some("The JavaScript language options to be supported"),
            ObjectValue(
                Map(
                    OptionalLabel("ecmaFeatures") -> NamedValueReference("properties_ecmaFeatures"),
                    OptionalLabel("ecmaVersion")  -> EnumValues("2015", "2016")
                )
            )
        )
        println(s"DocObjWithEnum: $docObjWithEnum")
        val docObjWithEnumPrint = SchemaSyntax.items.printString(docObjWithEnum)
        println(s"DocObjWithEnum print: $docObjWithEnumPrint")

        // Test empty EnumValues (this is what happens when non-string enums are lost)
        val emptyEnum      = EnumValues()
        println(s"EmptyEnum: $emptyEnum")
        val emptyEnumPrint = SchemaSyntax.items.printString(emptyEnum)
        println(s"EmptyEnum print: $emptyEnumPrint")

        // Test empty ObjectValue
        val emptyObj      = ObjectValue(Map.empty)
        println(s"EmptyObj: $emptyObj")
        val emptyObjPrint = SchemaSyntax.items.printString(emptyObj)
        println(s"EmptyObj print: $emptyObjPrint")

        assertTrue(refPrint.isRight && objPrint.isRight && ref2Print.isRight && docObjPrint.isRight && enumRefPrint.isRight && docObjWithEnumPrint.isRight)

    override def spec = suite("Sample Schemas Translation")(
        suite("JsonSchema imports (detailed)")(
            testJsonSchemaImportDetailed("npm.json"),
            testJsonSchemaImportDetailed("eslintrc.json")
        ),
        suite("JsonSchema imports")(
            testJsonSchemaImport("tsconfig.json")
        ),
        suite("XSD imports")(
            testXsdImport("wsdl.xsd"),
            testXsdImport("rss-2.0.xsd"),
            testXsdImport("atom.xsd"),
            testXsdImport("svg.xsd")
        )
    )

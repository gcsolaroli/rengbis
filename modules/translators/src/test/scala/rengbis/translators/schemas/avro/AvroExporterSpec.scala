package rengbis.translators.schemas.avro

import zio.test.*
import zio.json.*
import zio.json.ast.Json
import rengbis.Schema.*
import rengbis.translators.common.FrictionType

object AvroExporterSpec extends ZIOSpecDefault:

    def spec = suite("Avro Exporter")(
        suite("Primitive types")(
            test("exports boolean"):
                val schema         = BooleanValue()
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(json.toString.contains("boolean") || json.toString.contains("record"))
            ,
            test("exports text as string"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> TextValue()))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(json.toString.contains("string"))
            ,
            test("exports integer as int"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> NumericValue(Seq(NumericConstraint.Integer))))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(json.toString.contains("int"))
            ,
            test("exports number as double"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> NumericValue()))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(json.toString.contains("double"))
            ,
            test("exports binary as bytes"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> BinaryValue()))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(json.toString.contains("bytes"))
        ),
        suite("Complex types")(
            test("exports object as record"):
                val schema         = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        MandatoryLabel("age")  -> NumericValue(Seq(NumericConstraint.Integer))
                    )
                )
                val (json, report) = AvroExporter.toAvro(schema, "Person")
                assertTrue(
                    json.toString.contains("\"type\":\"record\"") &&
                        json.toString.contains("\"name\":\"Person\"") &&
                        report.entries.isEmpty
                )
            ,
            test("exports object with optional field"):
                val schema         = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        OptionalLabel("email") -> TextValue()
                    )
                )
                val (json, report) = AvroExporter.toAvro(schema, "User")
                assertTrue(
                    json.toString.contains("\"email\"") &&
                        json.toString.contains("[\"null\",\"string\"]")
                )
            ,
            test("exports list as array"):
                val schema         = ListOfValues(TextValue())
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    json.toString.contains("array") &&
                        json.toString.contains("\"items\":\"string\"")
                )
            ,
            test("exports map"):
                val schema         = MapValue(NumericValue(Seq(NumericConstraint.Integer)))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    json.toString.contains("map") &&
                        json.toString.contains("\"values\":\"int\"")
                )
            ,
            test("exports enum"):
                val schema         = EnumValues("RED", "GREEN", "BLUE")
                val (json, report) = AvroExporter.toAvro(schema, "Color")
                assertTrue(
                    json.toString.contains("\"type\":\"enum\"") &&
                        json.toString.contains("\"name\":\"Color\"") &&
                        json.toString.contains("\"symbols\":[\"RED\",\"GREEN\",\"BLUE\"]")
                )
        ),
        suite("Time types")(
            test("exports ISO8601 date as date logical type"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> TimeValue(TimeConstraint.NamedFormat.ISO8601_Date)))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    json.toString.contains("\"type\":\"int\"") &&
                        json.toString.contains("\"logicalType\":\"date\"")
                )
            ,
            test("exports ISO8601 time as time-millis logical type"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> TimeValue(TimeConstraint.NamedFormat.ISO8601_Time)))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    json.toString.contains("\"type\":\"int\"") &&
                        json.toString.contains("\"logicalType\":\"time-millis\"") &&
                        report.entries.exists(_.frictionType == FrictionType.Approximation)
                )
            ,
            test("exports ISO8601 timestamp as timestamp-millis logical type"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> TimeValue(TimeConstraint.NamedFormat.ISO8601)))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    json.toString.contains("\"type\":\"long\"") &&
                        json.toString.contains("\"logicalType\":\"timestamp-millis\"") &&
                        report.entries.exists(_.frictionType == FrictionType.Approximation)
                )
        ),
        suite("Alternatives and unions")(
            test("exports alternative values as union"):
                val schema         = AlternativeValues(TextValue(), NumericValue(Seq(NumericConstraint.Integer)))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(json.toString.contains("[\"string\",\"int\"]"))
        ),
        suite("Friction reporting")(
            test("reports friction for any type"):
                val schema         = AnyValue()
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    report.entries.exists(e =>
                        e.frictionType == FrictionType.Approximation &&
                            e.message.contains("any")
                    )
                )
            ,
            test("reports friction for text constraints"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> TextValue(Seq(TextConstraint.Size(BoundConstraint(BoundOp.MinInclusive, 5))))))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    report.entries.exists(e =>
                        e.frictionType == FrictionType.Loss &&
                            e.message.contains("Text constraints")
                    )
                )
            ,
            test("reports friction for numeric value constraints"):
                val schema         = ObjectValue(Map(MandatoryLabel("field") -> NumericValue(Seq(NumericConstraint.Value(BoundConstraint(BoundOp.MinInclusive, BigDecimal(0)))))))
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    report.entries.exists(e =>
                        e.frictionType == FrictionType.Loss &&
                            e.message.contains("Numeric value constraints")
                    )
                )
            ,
            test("reports friction for deprecated annotation"):
                val schema         = Deprecated(TextValue())
                val (json, report) = AvroExporter.toAvro(schema)
                assertTrue(
                    report.entries.exists(e =>
                        e.frictionType == FrictionType.Loss &&
                            e.message.contains("Deprecated")
                    )
                )
            ,
            test("reports friction for tuple conversion"):
                val schema         = TupleValue(TextValue(), NumericValue())
                val (json, report) = AvroExporter.toAvro(schema, "MyTuple")
                assertTrue(
                    report.entries.exists(e =>
                        e.frictionType == FrictionType.Approximation &&
                            e.message.contains("Tuple converted")
                    )
                )
        ),
        suite("Documentation")(
            test("preserves documentation on records"):
                val schema         = Documented(
                    Some("A person record"),
                    ObjectValue(
                        Map(
                            MandatoryLabel("name") -> TextValue()
                        )
                    )
                )
                val (json, report) = AvroExporter.toAvro(schema, "Person")
                json match
                    case Json.Obj(fields) =>
                        val jsonMap = fields.toMap
                        assertTrue(jsonMap.get("doc") == Some(Json.Str("A person record")))
                    case _                =>
                        assertTrue(false)
        )
    )

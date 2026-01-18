package rengbis.translators.schemas.avro

import zio.test.*
import zio.json.EncoderOps
import rengbis.Schema.*
import rengbis.translators.common.FrictionType

object AvroImporterSpec extends ZIOSpecDefault:

    def spec = suite("Avro Importer")(
        suite("Primitive types")(
            test("imports boolean"):
                val avro   = """"boolean""""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.map(_._1) == Right(BooleanValue()))
            ,
            test("imports int"):
                val avro   = """"int""""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.map(_._1) == Right(NumericValue(NumericConstraint.Constraints(integer = true))))
            ,
            test("imports long"):
                val avro   = """"long""""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.map(_._1) == Right(NumericValue(NumericConstraint.Constraints(integer = true))))
            ,
            test("imports float"):
                val avro   = """"float""""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.map(_._1) == Right(NumericValue()))
            ,
            test("imports double"):
                val avro   = """"double""""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.map(_._1) == Right(NumericValue()))
            ,
            test("imports string"):
                val avro   = """"string""""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.map(_._1) == Right(TextValue()))
            ,
            test("imports bytes"):
                val avro   = """"bytes""""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.map(_._1) == Right(BinaryValue()))
        ),
        suite("Complex types")(
            test("imports record"):
                val avro     = """{
                    "type": "record",
                    "name": "Person",
                    "fields": [
                        {"name": "name", "type": "string"},
                        {"name": "age", "type": "int"}
                    ]
                }"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        MandatoryLabel("age")  -> NumericValue(NumericConstraint.Constraints(integer = true))
                    )
                )
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports record with optional field"):
                val avro     = """{
                    "type": "record",
                    "name": "User",
                    "fields": [
                        {"name": "username", "type": "string"},
                        {"name": "email", "type": ["null", "string"], "default": null}
                    ]
                }"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = ObjectValue(
                    Map(
                        MandatoryLabel("username") -> TextValue(),
                        OptionalLabel("email")     -> TextValue()
                    )
                )
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports enum"):
                val avro     = """{
                    "type": "enum",
                    "name": "Color",
                    "symbols": ["RED", "GREEN", "BLUE"]
                }"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = EnumValues("RED", "GREEN", "BLUE")
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports array"):
                val avro     = """{
                    "type": "array",
                    "items": "string"
                }"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = ListOfValues(TextValue())
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports map"):
                val avro     = """{
                    "type": "map",
                    "values": "int"
                }"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = MapValue(NumericValue(NumericConstraint.Constraints(integer = true)))
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports fixed with friction"):
                val avro   = """{
                    "type": "fixed",
                    "name": "MD5",
                    "size": 16
                }"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(
                    result.map(_._1) == Right(BinaryValue()) &&
                        result.map(_._2.entries.exists(_.frictionType == FrictionType.Loss)).getOrElse(false)
                )
        ),
        suite("Logical types")(
            test("imports date logical type"):
                val avro     = """{"type": "int", "logicalType": "date"}"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = TimeValue(TimeConstraint.NamedFormat.ISO8601_Date)
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports time-millis logical type"):
                val avro     = """{"type": "int", "logicalType": "time-millis"}"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = TimeValue(TimeConstraint.NamedFormat.ISO8601_Time)
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports timestamp-millis logical type"):
                val avro     = """{"type": "long", "logicalType": "timestamp-millis"}"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = TimeValue(TimeConstraint.NamedFormat.ISO8601)
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports timestamp-micros with friction"):
                val avro   = """{"type": "long", "logicalType": "timestamp-micros"}"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(
                    result.map(_._1) == Right(TimeValue(TimeConstraint.NamedFormat.ISO8601)) &&
                        result.map(_._2.entries.exists(_.frictionType == FrictionType.Approximation)).getOrElse(false)
                )
            ,
            test("imports decimal with friction"):
                val avro   = """{"type": "bytes", "logicalType": "decimal", "precision": 10, "scale": 2}"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(
                    result.map(_._1) == Right(NumericValue()) &&
                        result.map(_._2.entries.exists(_.message.contains("decimal"))).getOrElse(false)
                )
            ,
            test("imports uuid with friction"):
                val avro   = """{"type": "string", "logicalType": "uuid"}"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(
                    result.map(_._1) == Right(TextValue()) &&
                        result.map(_._2.entries.exists(_.message.contains("UUID"))).getOrElse(false)
                )
        ),
        suite("Unions")(
            test("imports simple union"):
                val avro     = """["string", "int"]"""
                val result   = AvroImporter.fromAvro(avro)
                val expected = AlternativeValues(
                    TextValue(),
                    NumericValue(NumericConstraint.Constraints(integer = true))
                )
                assertTrue(result.map(_._1) == Right(expected))
            ,
            test("imports nullable union as optional"):
                val avro   = """["null", "string"]"""
                val result = AvroImporter.fromAvro(avro)
                // This is context-dependent; in field context it becomes optional
                assertTrue(result.isRight)
        ),
        suite("Documentation")(
            test("imports record with doc"):
                val avro   = """{
                    "type": "record",
                    "name": "Person",
                    "doc": "A person entity",
                    "fields": [
                        {"name": "name", "type": "string"}
                    ]
                }"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.isRight && (result.map(_._1) match {
                    case Right(Documented(Some(doc), _)) => doc == "A person entity"
                    case _                               => false
                }))
        ),
        suite("Nested structures")(
            test("imports nested record"):
                val avro   = """{
                    "type": "record",
                    "name": "Employee",
                    "fields": [
                        {
                            "name": "person",
                            "type": {
                                "type": "record",
                                "name": "Person",
                                "fields": [
                                    {"name": "name", "type": "string"},
                                    {"name": "age", "type": "int"}
                                ]
                            }
                        },
                        {"name": "employeeId", "type": "int"}
                    ]
                }"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.isRight)
            ,
            test("imports array of records"):
                val avro   = """{
                    "type": "array",
                    "items": {
                        "type": "record",
                        "name": "Item",
                        "fields": [
                            {"name": "id", "type": "int"},
                            {"name": "value", "type": "string"}
                        ]
                    }
                }"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.isRight && (result.map(_._1) match {
                    case Right(ListOfValues(ObjectValue(_), _)) => true
                    case _                                      => false
                }))
        ),
        suite("Error handling")(
            test("reports error for invalid JSON"):
                val avro   = """{"type": "invalid"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.isLeft)
            ,
            test("reports error for unknown type"):
                val avro   = """"unknownType""""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.isLeft)
            ,
            test("reports error for record without fields"):
                val avro   = """{"type": "record", "name": "Test"}"""
                val result = AvroImporter.fromAvro(avro)
                assertTrue(result.isLeft)
        ),
        suite("Round-trip compatibility")(
            test("round-trips simple record"):
                val original      = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        MandatoryLabel("age")  -> NumericValue(NumericConstraint.Constraints(integer = true))
                    )
                )
                val (avroJson, _) = AvroExporter.toAvro(original, "Person")
                val result        = AvroImporter.fromAvro(avroJson.toJsonPretty)
                assertTrue(result.map(_._1) == Right(original))
            ,
            test("round-trips enum"):
                val original      = EnumValues("A", "B", "C")
                val (avroJson, _) = AvroExporter.toAvro(original, "Letters")
                val result        = AvroImporter.fromAvro(avroJson.toJsonPretty)
                assertTrue(result.map(_._1) == Right(original))
            ,
            test("round-trips list"):
                val original      = ListOfValues(TextValue())
                val (avroJson, _) = AvroExporter.toAvro(original, "StringList")
                val result        = AvroImporter.fromAvro(avroJson.toJsonPretty)
                assertTrue(result.map(_._1) == Right(original))
        )
    )

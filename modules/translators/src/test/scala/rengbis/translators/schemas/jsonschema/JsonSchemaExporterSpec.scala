package rengbis.translators.schemas.jsonschema

import rengbis.Schema.*
import rengbis.translators.common.*
import zio.json.ast.Json
import zio.test.*

object JsonSchemaExporterSpec extends ZIOSpecDefault:

    def spec = suite("JsonSchemaExporter")(
        suite("Basic types")(
            test("exports TextValue") {
                val schema                   = TextValue()
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string")),
                    frictionReport.isEmpty
                )
            },
            test("exports NumericValue") {
                val schema                   = NumericValue()
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("number")),
                    frictionReport.isEmpty
                )
            },
            test("exports BooleanValue") {
                val schema                   = BooleanValue()
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("boolean")),
                    frictionReport.isEmpty
                )
            },
            test("exports AnyValue") {
                val schema                   = AnyValue()
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(),
                    frictionReport.isEmpty
                )
            },
            test("exports Fail") {
                val schema                   = Fail()
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("not" -> Json.Obj()),
                    frictionReport.isEmpty
                )
            },
            test("exports GivenTextValue") {
                val schema                   = GivenTextValue("hello")
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("const" -> Json.Str("hello")),
                    frictionReport.isEmpty
                )
            }
        ),
        suite("Text constraints")(
            test("exports size constraint (min)") {
                val schema                   = TextValue(Seq(TextConstraint.Size(BoundConstraint(BoundOp.MinInclusive, 5))))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "minLength" -> Json.Num(5)),
                    frictionReport.isEmpty
                )
            },
            test("exports size constraint (max)") {
                val schema                   = TextValue(Seq(TextConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, 100))))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "maxLength" -> Json.Num(100)),
                    frictionReport.isEmpty
                )
            },
            test("exports regex pattern") {
                val schema                   = TextValue(Seq(TextConstraint.Regex("^[a-z]+$")))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "pattern" -> Json.Str("^[a-z]+$")),
                    frictionReport.isEmpty
                )
            },
            test("exports email format") {
                val schema                   = TextValue(Seq(TextConstraint.Format("email")))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "format" -> Json.Str("email")),
                    frictionReport.isEmpty
                )
            },
            test("exports default value") {
                val schema                   = TextValue(Seq.empty, Some("default-text"))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "default" -> Json.Str("default-text")),
                    frictionReport.isEmpty
                )
            },
            test("reports friction for custom format") {
                val schema                   = TextValue(Seq(TextConstraint.Format("my-custom-format")))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string")),
                    frictionReport.nonEmpty,
                    frictionReport.entries.head.frictionType == FrictionType.Extension
                )
            }
        ),
        suite("Numeric constraints")(
            test("exports integer type") {
                val schema                   = NumericValue(Seq(NumericConstraint.Integer))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("integer")),
                    frictionReport.isEmpty
                )
            },
            test("exports minimum value") {
                val schema                   = NumericValue(Seq(NumericConstraint.Value(BoundConstraint(BoundOp.MinInclusive, BigDecimal(0)))))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("number"), "minimum" -> Json.Num(0)),
                    frictionReport.isEmpty
                )
            },
            test("exports exclusive minimum") {
                val schema                   = NumericValue(Seq(NumericConstraint.Value(BoundConstraint(BoundOp.MinExclusive, BigDecimal(0)))))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("number"), "exclusiveMinimum" -> Json.Num(0)),
                    frictionReport.isEmpty
                )
            },
            test("exports maximum value") {
                val schema                   = NumericValue(Seq(NumericConstraint.Value(BoundConstraint(BoundOp.MaxInclusive, BigDecimal(100)))))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("number"), "maximum" -> Json.Num(100)),
                    frictionReport.isEmpty
                )
            },
            test("exports exact value") {
                val schema                   = NumericValue(Seq(NumericConstraint.Value(BoundConstraint(BoundOp.Exact, BigDecimal(42)))))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("number"), "const" -> Json.Num(42)),
                    frictionReport.isEmpty
                )
            },
            test("exports default value") {
                val schema                   = NumericValue(Seq.empty, Some(BigDecimal(123)))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("number"), "default" -> Json.Num(123)),
                    frictionReport.isEmpty
                )
            }
        ),
        suite("Time constraints")(
            test("exports ISO8601 date-time") {
                val schema                   = TimeValue(TimeConstraint.NamedFormat.ISO8601)
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "format" -> Json.Str("date-time")),
                    frictionReport.isEmpty
                )
            },
            test("exports ISO8601 date") {
                val schema                   = TimeValue(TimeConstraint.NamedFormat.ISO8601_Date)
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "format" -> Json.Str("date")),
                    frictionReport.isEmpty
                )
            },
            test("exports ISO8601 time") {
                val schema                   = TimeValue(TimeConstraint.NamedFormat.ISO8601_Time)
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "format" -> Json.Str("time")),
                    frictionReport.isEmpty
                )
            },
            test("reports friction for custom pattern") {
                val schema                   = TimeValue(TimeConstraint.CustomPattern("yyyy-MM-dd"))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "format" -> Json.Str("date-time")),
                    frictionReport.nonEmpty,
                    frictionReport.entries.head.frictionType == FrictionType.Loss
                )
            }
        ),
        suite("List")(
            test("exports simple list") {
                val schema                   = ListOfValues(TextValue())
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(
                        "type"  -> Json.Str("array"),
                        "items" -> Json.Obj("type" -> Json.Str("string"))
                    ),
                    frictionReport.isEmpty
                )
            },
            test("exports list with minItems") {
                val schema                   = ListOfValues(TextValue(), ListConstraint.Size(BoundConstraint(BoundOp.MinInclusive, 1)))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(
                        "type"     -> Json.Str("array"),
                        "items"    -> Json.Obj("type" -> Json.Str("string")),
                        "minItems" -> Json.Num(1)
                    ),
                    frictionReport.isEmpty
                )
            },
            test("exports list with uniqueItems") {
                val schema                   = ListOfValues(TextValue(), ListConstraint.Unique)
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(
                        "type"        -> Json.Str("array"),
                        "items"       -> Json.Obj("type" -> Json.Str("string")),
                        "uniqueItems" -> Json.Bool(true)
                    ),
                    frictionReport.isEmpty
                )
            },
            test("reports friction for uniqueByFields") {
                val schema                   = ListOfValues(TextValue(), ListConstraint.UniqueByFields(Seq("field1", "field2")))
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    frictionReport.nonEmpty,
                    frictionReport.entries.head.frictionType == FrictionType.Loss
                )
            }
        ),
        suite("Tuple")(
            test("exports tuple") {
                val schema                   = TupleValue(TextValue(), NumericValue(), BooleanValue())
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(
                        "type"        -> Json.Str("array"),
                        "prefixItems" -> Json.Arr(
                            Json.Obj("type" -> Json.Str("string")),
                            Json.Obj("type" -> Json.Str("number")),
                            Json.Obj("type" -> Json.Str("boolean"))
                        ),
                        "items"       -> Json.Bool(false)
                    ),
                    frictionReport.isEmpty
                )
            }
        ),
        suite("Object")(
            test("exports simple object") {
                val schema                   = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        MandatoryLabel("age")  -> NumericValue()
                    )
                )
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(
                        "type"                 -> Json.Str("object"),
                        "properties"           -> Json.Obj(
                            "name" -> Json.Obj("type" -> Json.Str("string")),
                            "age"  -> Json.Obj("type" -> Json.Str("number"))
                        ),
                        "required"             -> Json.Arr(Json.Str("name"), Json.Str("age")),
                        "additionalProperties" -> Json.Bool(false)
                    ),
                    frictionReport.isEmpty
                )
            },
            test("exports object with optional field") {
                val schema                   = ObjectValue(
                    Map(
                        MandatoryLabel("name")    -> TextValue(),
                        OptionalLabel("nickname") -> TextValue()
                    )
                )
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                val properties               = result.get("properties").flatMap(_.asObject).get
                val required                 = result.get("required").flatMap(_.asArray).get
                assertTrue(
                    properties.get("name").isDefined,
                    properties.get("nickname").isDefined,
                    required.size == 1,
                    required.contains(Json.Str("name")),
                    frictionReport.isEmpty
                )
            }
        ),
        suite("Map")(
            test("exports map") {
                val schema                   = MapValue(NumericValue())
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(
                        "type"                 -> Json.Str("object"),
                        "additionalProperties" -> Json.Obj("type" -> Json.Str("number"))
                    ),
                    frictionReport.isEmpty
                )
            }
        ),
        suite("Enum")(
            test("exports enum values") {
                val schema                   = EnumValues("red", "green", "blue")
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(
                        "enum" -> Json.Arr(Json.Str("red"), Json.Str("green"), Json.Str("blue"))
                    ),
                    frictionReport.isEmpty
                )
            }
        ),
        suite("Alternative values")(
            test("exports alternative values as anyOf") {
                val schema                   = AlternativeValues(TextValue(), NumericValue())
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj(
                        "anyOf" -> Json.Arr(
                            Json.Obj("type" -> Json.Str("string")),
                            Json.Obj("type" -> Json.Str("number"))
                        )
                    ),
                    frictionReport.isEmpty
                )
            }
        ),
        suite("References")(
            test("exports named reference") {
                val schema                   = NamedValueReference("MyType")
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("$ref" -> Json.Str("#/$defs/MyType")),
                    frictionReport.isEmpty
                )
            },
            test("exports scoped reference") {
                val schema                   = ScopedReference("mymodule", "MyType")
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("$ref" -> Json.Str("#/$defs/mymodule.MyType")),
                    frictionReport.isEmpty
                )
            }
        ),
        suite("Documented and Deprecated")(
            test("exports documented schema") {
                val schema                   = Documented(Some("This is a text field"), TextValue())
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "description" -> Json.Str("This is a text field")),
                    frictionReport.isEmpty
                )
            },
            test("exports deprecated schema") {
                val schema                   = Deprecated(TextValue())
                val (result, frictionReport) = JsonSchemaExporter.toJsonSchema(schema)
                assertTrue(
                    result == Json.Obj("type" -> Json.Str("string"), "deprecated" -> Json.Bool(true)),
                    frictionReport.isEmpty
                )
            }
        )
    )

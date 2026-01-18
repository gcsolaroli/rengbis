package rengbis.translators.schemas.jsonschema

import rengbis.Schema.*
import rengbis.translators.common.*
import zio.json.EncoderOps
import zio.test.*

object JsonSchemaImporterSpec extends ZIOSpecDefault:

    def spec = suite("JsonSchemaImporter")(
        suite("Basic types")(
            test("imports string type") {
                val jsonSchema = """{"type": "string"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TextValue())
                )
            },
            test("imports number type") {
                val jsonSchema = """{"type": "number"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(NumericValue())
                )
            },
            test("imports integer type") {
                val jsonSchema = """{"type": "integer"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(NumericValue(NumericConstraint.Constraints(integer = true)))
                )
            },
            test("imports boolean type") {
                val jsonSchema = """{"type": "boolean"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(BooleanValue())
                )
            },
            test("imports true schema as AnyValue") {
                val jsonSchema = """true"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(AnyValue())
                )
            },
            test("imports false schema as Fail") {
                val jsonSchema = """false"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(Fail())
                )
            }
        ),
        suite("String constraints")(
            test("imports minLength") {
                val jsonSchema = """{"type": "string", "minLength": 5}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TextValue(TextConstraint.Constraints(size = Some(TextConstraint.SizeRange.minInclusive(5)))))
                )
            },
            test("imports maxLength") {
                val jsonSchema = """{"type": "string", "maxLength": 100}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TextValue(TextConstraint.Constraints(size = Some(TextConstraint.SizeRange.maxInclusive(100)))))
                )
            },
            test("imports pattern") {
                val jsonSchema = """{"type": "string", "pattern": "^[a-z]+$"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TextValue(TextConstraint.Constraints(regex = Some("^[a-z]+$"))))
                )
            },
            test("imports email format") {
                val jsonSchema = """{"type": "string", "format": "email"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TextValue(TextConstraint.Constraints(format = Some("email"))))
                )
            },
            test("imports string with default") {
                val jsonSchema = """{"type": "string", "default": "hello"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TextValue(default = Some("hello")))
                )
            }
        ),
        suite("Number constraints")(
            test("imports minimum") {
                val jsonSchema = """{"type": "number", "minimum": 0}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.minInclusive(BigDecimal(0))))))
                )
            },
            test("imports maximum") {
                val jsonSchema = """{"type": "number", "maximum": 100}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.maxInclusive(BigDecimal(100))))))
                )
            },
            test("imports exclusiveMinimum") {
                val jsonSchema = """{"type": "number", "exclusiveMinimum": 0}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.minExclusive(BigDecimal(0))))))
                )
            },
            test("imports exclusiveMaximum") {
                val jsonSchema = """{"type": "number", "exclusiveMaximum": 100}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.maxExclusive(BigDecimal(100))))))
                )
            },
            test("imports integer with range") {
                val jsonSchema = """{"type": "integer", "minimum": 1, "maximum": 10}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = NumericValue(
                    NumericConstraint.Constraints(
                        value = Some(
                            NumericConstraint.ValueRange(
                                Some(BoundConstraint(BoundOp.MinInclusive, BigDecimal(1))),
                                Some(BoundConstraint(BoundOp.MaxInclusive, BigDecimal(10)))
                            )
                        ),
                        integer = true
                    )
                )
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            },
            test("reports friction for multipleOf") {
                val jsonSchema               = """{"type": "number", "multipleOf": 5}"""
                val result                   = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val (schema, frictionReport) = result.getOrElse((AnyValue(), FrictionReport()))
                assertTrue(
                    result.isRight,
                    schema == NumericValue(),
                    frictionReport.nonEmpty,
                    frictionReport.entries.exists(_.frictionType == FrictionType.Loss)
                )
            }
        ),
        suite("Time/Date formats")(
            test("imports date-time format") {
                val jsonSchema = """{"type": "string", "format": "date-time"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TimeValue(TimeConstraint.NamedFormat.ISO8601_DateTime))
                )
            },
            test("imports date format") {
                val jsonSchema = """{"type": "string", "format": "date"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TimeValue(TimeConstraint.NamedFormat.ISO8601_Date))
                )
            },
            test("imports time format") {
                val jsonSchema = """{"type": "string", "format": "time"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TimeValue(TimeConstraint.NamedFormat.ISO8601_Time))
                )
            }
        ),
        suite("Arrays")(
            test("imports simple array") {
                val jsonSchema = """{"type": "array", "items": {"type": "string"}}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(ListOfValues(TextValue()))
                )
            },
            test("imports array with minItems") {
                val jsonSchema = """{"type": "array", "items": {"type": "string"}, "minItems": 1}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = ListOfValues(TextValue(), ListConstraint.Constraints(size = Some(ListConstraint.SizeRange.minInclusive(1))))
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            },
            test("imports array with uniqueItems") {
                val jsonSchema = """{"type": "array", "items": {"type": "string"}, "uniqueItems": true}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = ListOfValues(TextValue(), ListConstraint.Constraints(unique = Seq(ListConstraint.Uniqueness.Simple)))
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            }
        ),
        suite("Tuples")(
            test("imports tuple with prefixItems") {
                val jsonSchema = """{"type": "array", "prefixItems": [{"type": "string"}, {"type": "number"}], "items": false}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TupleValue(TextValue(), NumericValue()))
                )
            }
        ),
        suite("Objects")(
            test("imports simple object") {
                val jsonSchema =
                    """{"type": "object", "properties": {"name": {"type": "string"}, "age": {"type": "number"}}, "required": ["name", "age"]}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        MandatoryLabel("age")  -> NumericValue()
                    )
                )
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            },
            test("imports object with optional field") {
                val jsonSchema =
                    """{"type": "object", "properties": {"name": {"type": "string"}, "nickname": {"type": "string"}}, "required": ["name"]}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = ObjectValue(
                    Map(
                        MandatoryLabel("name")    -> TextValue(),
                        OptionalLabel("nickname") -> TextValue()
                    )
                )
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            }
        ),
        suite("Maps")(
            test("imports map with additionalProperties") {
                val jsonSchema = """{"type": "object", "additionalProperties": {"type": "number"}}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(MapValue(NumericValue()))
                )
            }
        ),
        suite("Enums and Constants")(
            test("imports enum") {
                val jsonSchema = """{"enum": ["red", "green", "blue"]}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(EnumValues("red", "green", "blue"))
                )
            },
            test("imports const string") {
                val jsonSchema = """{"const": "value"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(GivenTextValue("value"))
                )
            },
            test("imports const number") {
                val jsonSchema = """{"const": 42}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.exact(BigDecimal(42)))))
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            }
        ),
        suite("Composition")(
            test("imports anyOf") {
                val jsonSchema = """{"anyOf": [{"type": "string"}, {"type": "number"}]}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(AlternativeValues(TextValue(), NumericValue()))
                )
            },
            test("imports oneOf with friction") {
                val jsonSchema               = """{"oneOf": [{"type": "string"}, {"type": "number"}]}"""
                val result                   = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val (schema, frictionReport) = result.getOrElse((AnyValue(), FrictionReport()))
                assertTrue(
                    result.isRight,
                    schema == AlternativeValues(TextValue(), NumericValue()),
                    frictionReport.nonEmpty,
                    frictionReport.entries.exists(_.frictionType == FrictionType.Approximation)
                )
            },
            test("reports friction for not") {
                val jsonSchema               = """{"not": {"type": "string"}}"""
                val result                   = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val (schema, frictionReport) = result.getOrElse((AnyValue(), FrictionReport()))
                assertTrue(
                    result.isRight,
                    schema == AnyValue(),
                    frictionReport.nonEmpty,
                    frictionReport.entries.exists(_.frictionType == FrictionType.Loss)
                )
            }
        ),
        suite("References")(
            test("imports internal $ref") {
                val jsonSchema = """{"$ref": "#/$defs/MyType"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(NamedValueReference("MyType"))
                )
            },
            test("imports file-based $ref") {
                val jsonSchema = """{"$ref": "./person.json#/$defs/Address"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists(_.isInstanceOf[ScopedReference])
                )
            }
        ),
        suite("Metadata")(
            test("imports description") {
                val jsonSchema = """{"type": "string", "description": "A text field"}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(Documented(Some("A text field"), TextValue()))
                )
            },
            test("imports deprecated") {
                val jsonSchema = """{"type": "string", "deprecated": true}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(Deprecated(TextValue()))
                )
            },
            test("imports both description and deprecated") {
                val jsonSchema = """{"type": "string", "description": "Old field", "deprecated": true}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(Deprecated(Documented(Some("Old field"), TextValue())))
                )
            }
        ),
        suite("Type arrays")(
            test("imports type array as AlternativeValues") {
                val jsonSchema               = """{"type": ["string", "number"]}"""
                val result                   = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val (schema, frictionReport) = result.getOrElse((AnyValue(), FrictionReport()))
                assertTrue(
                    result.isRight,
                    schema == AlternativeValues(TextValue(), NumericValue()),
                    frictionReport.nonEmpty,
                    frictionReport.entries.exists(_.frictionType == FrictionType.Approximation)
                )
            }
        ),
        suite("Round-trip compatibility")(
            test("string with constraints round-trips") {
                val original      = TextValue(TextConstraint.Constraints(size = Some(TextConstraint.SizeRange.minInclusive(5))))
                val (exported, _) = JsonSchemaExporter.toJsonSchema(original)
                val reimported    = JsonSchemaImporter.fromJsonSchema(exported.toJson)
                assertTrue(
                    reimported.isRight,
                    reimported.map(_._1) == Right(original)
                )
            },
            test("number with constraints round-trips") {
                val original      = NumericValue(
                    NumericConstraint.Constraints(
                        value = Some(NumericConstraint.ValueRange.minInclusive(BigDecimal(0))),
                        integer = true
                    )
                )
                val (exported, _) = JsonSchemaExporter.toJsonSchema(original)
                val reimported    = JsonSchemaImporter.fromJsonSchema(exported.toJson)
                assertTrue(
                    reimported.isRight,
                    reimported.map(_._1) == Right(original)
                )
            },
            test("object round-trips") {
                val original      = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        OptionalLabel("age")   -> NumericValue()
                    )
                )
                val (exported, _) = JsonSchemaExporter.toJsonSchema(original)
                val reimported    = JsonSchemaImporter.fromJsonSchema(exported.toJson)
                assertTrue(
                    reimported.isRight,
                    reimported.map(_._1) == Right(original)
                )
            }
        )
    )

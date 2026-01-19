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
            test("imports oneOf as AlternativeValues") {
                // oneOf with non-overlapping types maps exactly to AlternativeValues - no friction
                val jsonSchema = """{"oneOf": [{"type": "string"}, {"type": "number"}]}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(AlternativeValues(TextValue(), NumericValue())),
                    result.map(_._2.isEmpty) == Right(true) // No friction expected
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
            },
            test("deep $ref path resolves inline instead of creating NamedValueReference") {
                // This tests the tsconfig.json pattern where a deep ref like
                // #/definitions/compilerOptionsDefinition/properties/compilerOptions
                // should be resolved by following the path, not by extracting "compilerOptions"
                // as a definition name (which would be incorrect)
                val jsonSchema = """{
                    "$ref": "#/definitions/parentDef/properties/nestedProp",
                    "definitions": {
                        "parentDef": {
                            "properties": {
                                "nestedProp": {
                                    "type": "object",
                                    "properties": {
                                        "value": { "type": "string" }
                                    }
                                }
                            }
                        }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                // Should resolve to the nested object, not to NamedValueReference("nestedProp")
                // which would cause "Referenced definition 'nestedProp' not found" friction
                val expected   = ObjectValue(
                    Map(
                        OptionalLabel("value") -> TextValue()
                    )
                )
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            },
            test("deep $ref path does not report missing definition friction") {
                // When we have a deep $ref, we should NOT get a friction report about
                // "Referenced definition 'X' not found" because X is not a definition
                val jsonSchema               = """{
                    "$ref": "#/definitions/parentDef/properties/nested",
                    "definitions": {
                        "parentDef": {
                            "properties": {
                                "nested": { "type": "string" }
                            }
                        }
                    }
                }"""
                val result                   = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val (schema, frictionReport) = result.getOrElse((AnyValue(), FrictionReport()))
                assertTrue(
                    result.isRight,
                    schema == TextValue(),
                    // Should NOT have friction about missing definition
                    !frictionReport.entries.exists(e => e.message.contains("Referenced definition") && e.message.contains("not found"))
                )
            }
        ),
        suite("Metadata")(
            test("imports description on object type") {
                val jsonSchema  = """{"type": "object", "description": "An object", "properties": {"x": {"type": "string"}}}"""
                val result      = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val (schema, _) = result.getOrElse((AnyValue(), FrictionReport()))
                assertTrue(
                    result.isRight,
                    schema.isInstanceOf[Documented],
                    schema.asInstanceOf[Documented].doc == Some("An object")
                )
            },
            test("description on non-object type is preserved") {
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
            test("imports both deprecated and description") {
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
                // Type arrays map exactly to AlternativeValues - no friction
                val jsonSchema = """{"type": ["string", "number"]}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(AlternativeValues(TextValue(), NumericValue())),
                    result.map(_._2.isEmpty) == Right(true) // No friction expected
                )
            },
            test("imports type array [array, null] with items as list (null handled via optionality)") {
                // This is the filesDefinition case from tsconfig.json
                val jsonSchema = """{
                    "type": ["array", "null"],
                    "uniqueItems": true,
                    "items": {
                        "type": "string"
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = ListOfValues(TextValue(), ListConstraint.Constraints(unique = Seq(ListConstraint.Uniqueness.Simple)))
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            },
            test("imports type array [array, null] preserves items constraint") {
                val jsonSchema = """{
                    "type": ["array", "null"],
                    "items": {
                        "type": "number"
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(ListOfValues(NumericValue()))
                )
            },
            test("imports single non-null type from array without AlternativeValues wrapper") {
                // When type array has only one non-null type, don't wrap in AlternativeValues
                val jsonSchema = """{"type": ["string", "null"]}"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(TextValue())
                )
            }
        ),
        suite("Type arrays with description")(
            test("imports type array with description preserves documentation") {
                // The filesDefinition case: description should be preserved on the array type
                val jsonSchema = """{
                    "description": "List of files to include",
                    "type": ["array", "null"],
                    "items": {
                        "type": "string"
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = Documented(Some("List of files to include"), ListOfValues(TextValue()))
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            }
        ),
        suite("allOf with multiple $ref")(
            test("imports allOf with multiple $ref by merging inlined definitions") {
                // Simplified version of tsconfig.json pattern
                val jsonSchema = """{
                    "type": "object",
                    "allOf": [
                        { "$ref": "#/definitions/def1" },
                        { "$ref": "#/definitions/def2" }
                    ],
                    "definitions": {
                        "def1": {
                            "properties": {
                                "name": { "type": "string" }
                            }
                        },
                        "def2": {
                            "properties": {
                                "age": { "type": "number" }
                            }
                        }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                val expected   = ObjectValue(
                    Map(
                        OptionalLabel("name") -> TextValue(),
                        OptionalLabel("age")  -> NumericValue()
                    )
                )
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            },
            test("imports allOf with $ref and anyOf") {
                // Pattern from tsconfig.json: allOf with refs and an anyOf
                // All properties from all definitions (including those inside anyOf) should be merged
                val jsonSchema = """{
                    "type": "object",
                    "allOf": [
                        { "$ref": "#/definitions/def1" },
                        {
                            "anyOf": [
                                { "$ref": "#/definitions/def2" },
                                { "$ref": "#/definitions/def3" }
                            ]
                        }
                    ],
                    "definitions": {
                        "def1": {
                            "properties": {
                                "name": { "type": "string" }
                            }
                        },
                        "def2": {
                            "properties": {
                                "files": { "type": "array", "items": { "type": "string" } }
                            }
                        },
                        "def3": {
                            "properties": {
                                "include": { "type": "array", "items": { "type": "string" } }
                            }
                        }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchema(jsonSchema)
                // All properties from def1, def2, and def3 should be merged into the root object
                val expected   = ObjectValue(
                    Map(
                        OptionalLabel("name")    -> TextValue(),
                        OptionalLabel("files")   -> ListOfValues(TextValue()),
                        OptionalLabel("include") -> ListOfValues(TextValue())
                    )
                )
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(expected)
                )
            },
            test("allOf with anyOf does not create dangling definitions for inlined refs") {
                // When refs inside anyOf are inlined into the allOf merge, they should NOT
                // appear as separate named definitions
                val jsonSchema = """{
                    "type": "object",
                    "allOf": [
                        { "$ref": "#/definitions/def1" },
                        {
                            "anyOf": [
                                { "$ref": "#/definitions/def2" },
                                { "$ref": "#/definitions/def3" }
                            ]
                        }
                    ],
                    "definitions": {
                        "def1": {
                            "properties": {
                                "name": { "type": "string" }
                            }
                        },
                        "def2": {
                            "properties": {
                                "files": { "type": "array", "items": { "type": "string" } }
                            }
                        },
                        "def3": {
                            "properties": {
                                "include": { "type": "array", "items": { "type": "string" } }
                            }
                        }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema)
                // No named definitions should be output - all are inlined
                assertTrue(
                    result.isRight,
                    result.map(_.definitions.isEmpty) == Right(true)
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

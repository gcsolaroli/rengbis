package rengbis

import zio.test.{ assertTrue, ZIOSpecDefault }
import rengbis.Schema.{ AlternativeValues, AnyValue, BinaryValue, BooleanValue, Deprecated, Documented, EnumValues, GivenTextValue, ListOfValues, MandatoryLabel, MapValue, NumericValue, ObjectValue, OptionalLabel, Schema, TextValue, TimeValue, TupleValue }
import rengbis.Schema.{ BinaryConstraint, ListConstraint, NumericConstraint, TextConstraint, TimeConstraint }

object SchemaSerializationSpec extends ZIOSpecDefault:

    // Helper to create TextValue with size constraints
    def textWithSize(size: TextConstraint.SizeRange): TextValue =
        TextValue(TextConstraint.Constraints(size = Some(size)))

    def textWithRegex(pattern: String): TextValue =
        TextValue(TextConstraint.Constraints(regex = Some(pattern)))

    def textWithFormat(format: String): TextValue =
        TextValue(TextConstraint.Constraints(format = Some(format)))

    def textWithDefault(default: String): TextValue =
        TextValue(TextConstraint.Constraints.empty, Some(default))

    def textWithSizeAndDefault(size: TextConstraint.SizeRange, default: String): TextValue =
        TextValue(TextConstraint.Constraints(size = Some(size)), Some(default))

    // Helper to create NumericValue with constraints
    def numWithInteger: NumericValue =
        NumericValue(NumericConstraint.Constraints(integer = true))

    def numWithValue(value: NumericConstraint.ValueRange): NumericValue =
        NumericValue(NumericConstraint.Constraints(value = Some(value)))

    def numWithDefault(default: BigDecimal): NumericValue =
        NumericValue(NumericConstraint.Constraints.empty, Some(default))

    def numWithIntegerAndDefault(default: BigDecimal): NumericValue =
        NumericValue(NumericConstraint.Constraints(integer = true), Some(default))

    // Helper to create BinaryValue with constraints
    def binaryWithEncoding(enc: BinaryConstraint.BinaryToTextEncoder): BinaryValue =
        BinaryValue(BinaryConstraint.Constraints(encoding = Some(enc)))

    def binaryWithSize(size: BinaryConstraint.SizeRange): BinaryValue =
        BinaryValue(BinaryConstraint.Constraints(size = Some(size)))

    // Helper to create ListOfValues with constraints
    def listWithSize(schema: Schema, size: ListConstraint.SizeRange): ListOfValues =
        ListOfValues(schema, ListConstraint.Constraints(size = Some(size)))

    def listWithUnique(schema: Schema): ListOfValues =
        ListOfValues(schema, ListConstraint.Constraints(unique = Seq(ListConstraint.Uniqueness.Simple)))

    def spec = suite("Schema serialization")(
        suite("Basic types")(
            printTest(AnyValue(), "any"),
            printTest(BooleanValue(), "boolean"),
            printTest(TextValue(), "text"),
            printTest(NumericValue(), "number"),
            printTest(BinaryValue(), "binary")
        ),
        suite("Text with constraints")(
            printTest(textWithSize(TextConstraint.SizeRange.exact(10)), "text [ length == 10 ]"),
            printTest(textWithSize(TextConstraint.SizeRange.minInclusive(10)), "text [ length >= 10 ]"),
            printTest(textWithSize(TextConstraint.SizeRange.maxInclusive(100)), "text [ length <= 100 ]"),
            printTest(textWithSize(TextConstraint.SizeRange.minExclusive(10)), "text [ length > 10 ]"),
            printTest(textWithSize(TextConstraint.SizeRange.maxExclusive(100)), "text [ length < 100 ]"),
            printTest(textWithRegex("^[a-z]+$"), """text [ regex = "^[a-z]+$" ]"""),
            printTest(textWithFormat("###-####"), """text [ pattern = "###-####" ]"""),
            printTest(textWithDefault("active"), """text ?= "active""""),
            printTest(textWithSizeAndDefault(TextConstraint.SizeRange.maxInclusive(100), "default"), """text [ length <= 100 ] ?= "default"""")
        ),
        suite("Numeric with constraints")(
            printTest(numWithInteger, "number [ integer ]"),
            printTest(numWithValue(NumericConstraint.ValueRange.minInclusive(0)), "number [ value >= 0 ]"),
            printTest(numWithValue(NumericConstraint.ValueRange.maxInclusive(100)), "number [ value <= 100 ]"),
            printTest(numWithValue(NumericConstraint.ValueRange.exact(42)), "number [ value == 42 ]"),
            printTest(numWithDefault(BigDecimal(0)), "number ?= 0"),
            printTest(numWithIntegerAndDefault(BigDecimal(42)), "number [ integer ] ?= 42")
        ),
        suite("Binary with constraints")(
            printTest(binaryWithEncoding(BinaryConstraint.BinaryToTextEncoder.base64), "binary [ encoding = 'base64' ]"),
            printTest(binaryWithEncoding(BinaryConstraint.BinaryToTextEncoder.hex), "binary [ encoding = 'hex' ]"),
            printTest(binaryWithSize(BinaryConstraint.SizeRange.exact(32)), "binary [ bytes == 32 ]")
        ),
        suite("Time with constraints")(
            printTest(TimeValue(TimeConstraint.NamedFormat.ISO8601), "time [ format = 'iso8601' ]"),
            printTest(TimeValue(TimeConstraint.NamedFormat.ISO8601_Date), "time [ format = 'iso8601-date' ]"),
            printTest(TimeValue(TimeConstraint.NamedFormat.ISO8601_Time), "time [ format = 'iso8601-time' ]"),
            printTest(TimeValue(TimeConstraint.NamedFormat.RFC3339), "time [ format = 'rfc3339' ]"),
            printTest(TimeValue(TimeConstraint.CustomPattern("yyyy-MM-dd")), """time [ format = "yyyy-MM-dd" ]"""),
            printTest(TimeValue(TimeConstraint.CustomPattern("HH:mm:ss")), """time [ format = "HH:mm:ss" ]""")
        ),
        suite("Lists")(
            printTest(ListOfValues(TextValue()), "text*"),
            printTest(ListOfValues(NumericValue()), "number*"),
            printTest(listWithSize(TextValue(), ListConstraint.SizeRange.minInclusive(1)), "text+"),
            printTest(listWithSize(TextValue(), ListConstraint.SizeRange.exact(5)), "text* [ size == 5 ]"),
            printTest(listWithUnique(TextValue()), "text* [ unique ]")
        ),
        suite("Alternatives and enums")(
            printTest(AlternativeValues(TextValue(), NumericValue()), "text | number"),
            printTest(EnumValues("yes", "no"), """"yes" | "no"""")
        ),
        suite("Objects")(
            printTest(
                ObjectValue(Map(MandatoryLabel("name") -> TextValue())),
                "{ name: text }"
            ),
            printTest(
                ObjectValue(Map(OptionalLabel("name") -> TextValue())),
                "{ name?: text }"
            )
        ),
        suite("Maps")(
            printTest(MapValue(TextValue()), "{ ...: text }")
        ),
        suite("Tuples")(
            printTest(TupleValue(TextValue(), NumericValue()), "(text, number)")
        ),
        suite("Documentation comments")(
            printTest(
                ObjectValue(Map(MandatoryLabel("name") -> Documented(Some("The name"), TextValue()))),
                "{ ## The name\nname: text }"
            ),
            test("roundtrip: object field with doc comment (normalized to object doc)"):
                val schema   = ObjectValue(Map(MandatoryLabel("name") -> Documented(Some("The name"), TextValue())))
                val expected = Documented(Some("The name"), ObjectValue(Map(MandatoryLabel("name") -> TextValue())))
                assertTrue(roundTrip(schema) == Right(expected))
        ),
        suite("Deprecated annotation")(
            printTest(
                ObjectValue(Map(MandatoryLabel("old") -> Deprecated(TextValue()))),
                "{ @deprecated old: text }"
            ),
            roundTripTest(
                ObjectValue(Map(MandatoryLabel("old") -> Deprecated(TextValue()))),
                "deprecated object field"
            ),
            test("roundtrip: deprecated object field with doc comment (normalized to object doc)"):
                val schema   = ObjectValue(Map(MandatoryLabel("old") -> Deprecated(Documented(Some("Old field"), TextValue()))))
                val expected = Documented(Some("Old field"), ObjectValue(Map(MandatoryLabel("old") -> Deprecated(TextValue()))))
                assertTrue(roundTrip(schema) == Right(expected))
        ),
        suite("Mixed text constraints")(
            // This test verifies: text with both Size and Regex constraints can round-trip
            roundTripTest(
                TextValue(
                    TextConstraint.Constraints(
                        size = Some(
                            TextConstraint.SizeRange(
                                min = Some(Schema.BoundConstraint(Schema.BoundOp.MinInclusive, 1)),
                                max = Some(Schema.BoundConstraint(Schema.BoundOp.MaxInclusive, 100))
                            )
                        ),
                        regex = Some("^[a-z]+$")
                    )
                ),
                "text with mixed size and regex constraints"
            )
        ),
        suite("Round-trip tests")(
            roundTripTest(AnyValue(), "any"),
            roundTripTest(TextValue(), "text"),
            roundTripTest(NumericValue(), "number"),
            roundTripTest(BooleanValue(), "boolean"),
            roundTripTest(BinaryValue(), "binary"),
            roundTripTest(
                TextValue(
                    TextConstraint.Constraints(size =
                        Some(
                            TextConstraint.SizeRange(
                                min = Some(Schema.BoundConstraint(Schema.BoundOp.MinInclusive, 10)),
                                max = Some(Schema.BoundConstraint(Schema.BoundOp.MaxInclusive, 100))
                            )
                        )
                    )
                ),
                "text with length constraints"
            ),
            roundTripTest(
                NumericValue(
                    NumericConstraint.Constraints(
                        value = Some(NumericConstraint.ValueRange.minInclusive(0)),
                        integer = true
                    )
                ),
                "integer with min value"
            ),
            roundTripTest(ListOfValues(TextValue()), "list of text"),
            roundTripTest(listWithSize(NumericValue(), ListConstraint.SizeRange.minInclusive(1)), "non-empty list"),
            roundTripTest(AlternativeValues(TextValue(), NumericValue()), "alternatives"),
            roundTripTest(EnumValues("a", "b", "c"), "enum values"),
            roundTripTest(
                ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        OptionalLabel("age")   -> NumericValue()
                    )
                ),
                "object with mandatory and optional fields"
            ),
            roundTripTest(MapValue(NumericValue()), "map with numeric values"),
            roundTripTest(TupleValue(TextValue(), NumericValue(), BooleanValue()), "tuple"),
            roundTripTest(textWithDefault("default"), "text with default"),
            roundTripTest(textWithSizeAndDefault(TextConstraint.SizeRange.maxInclusive(100), "default"), "text with constraints and default"),
            roundTripTest(numWithDefault(BigDecimal(0)), "number with default"),
            roundTripTest(numWithIntegerAndDefault(BigDecimal(42)), "number with constraints and default"),
            roundTripTest(TimeValue(TimeConstraint.NamedFormat.ISO8601), "time with iso8601"),
            roundTripTest(TimeValue(TimeConstraint.NamedFormat.ISO8601_Date), "time with iso8601-date"),
            roundTripTest(TimeValue(TimeConstraint.CustomPattern("yyyy-MM-dd")), "time with custom pattern")
        )
    )

    // ------------------------------------------------------------------------

    def print(schema: Schema): Either[String, String] =
        SchemaSyntax.items.printString(schema).left.map(_.toString)

    def roundTrip(schema: Schema): Either[String, Schema] =
        for
            printed <- print(schema)
            parsed  <- SchemaLoader.parseSchema(s"= $printed").flatMap(_.getRootSchema)
        yield parsed

    def printTest(schema: Schema, expectedOutput: String) =
        test(s"print: $expectedOutput"):
            assertTrue(print(schema) == Right(expectedOutput))

    def roundTripTest(schema: Schema, description: String) =
        test(s"roundtrip: $description"):
            assertTrue(roundTrip(schema) == Right(schema))

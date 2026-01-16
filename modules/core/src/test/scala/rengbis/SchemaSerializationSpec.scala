package rengbis

import zio.test.{ assertTrue, ZIOSpecDefault }
import rengbis.Schema.{ AlternativeValues, AnyValue, BinaryValue, BooleanValue, EnumValues, GivenTextValue, ListOfValues, MandatoryLabel, MapValue, NumericValue, ObjectValue, OptionalLabel, Schema, TextValue, TupleValue }
import rengbis.Schema.{ BinaryConstraint, ListConstraint, NumericConstraint, TextConstraint }
import rengbis.testHelpers.{ binBytes, listSize, numValue, textLength }

object SchemaSerializationSpec extends ZIOSpecDefault:

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

    def spec = suite("Schema serialization")(
        suite("Basic types")(
            printTest(AnyValue(), "any"),
            printTest(BooleanValue(), "boolean"),
            printTest(TextValue(), "text"),
            printTest(NumericValue(), "number"),
            printTest(BinaryValue(), "binary")
        ),
        suite("Text with constraints")(
            printTest(TextValue(textLength === 10), "text [ length == 10 ]"),
            printTest(TextValue(textLength >= 10), "text [ length >= 10 ]"),
            printTest(TextValue(textLength <= 100), "text [ length <= 100 ]"),
            printTest(TextValue(textLength > 10), "text [ length > 10 ]"),
            printTest(TextValue(textLength < 100), "text [ length < 100 ]"),
            printTest(TextValue(TextConstraint.Regex("^[a-z]+$")), """text [ regex = "^[a-z]+$" ]"""),
            printTest(TextValue(TextConstraint.Format("###-####")), """text [ pattern = "###-####" ]""")
        ),
        suite("Numeric with constraints")(
            printTest(NumericValue(NumericConstraint.Integer), "number [ integer ]"),
            printTest(NumericValue(numValue >= 0), "number [ value >= 0 ]"),
            printTest(NumericValue(numValue <= 100), "number [ value <= 100 ]"),
            printTest(NumericValue(numValue === 42), "number [ value == 42 ]")
        ),
        suite("Binary with constraints")(
            printTest(BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.base64)), "binary [ encoding = 'base64' ]"),
            printTest(BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.hex)), "binary [ encoding = 'hex' ]"),
            printTest(BinaryValue(binBytes === 32), "binary [ bytes == 32 ]")
        ),
        suite("Lists")(
            printTest(ListOfValues(TextValue()), "text*"),
            printTest(ListOfValues(NumericValue()), "number*"),
            printTest(ListOfValues(TextValue(), listSize >= 1), "text+"),
            printTest(ListOfValues(TextValue(), listSize === 5), "text* [ size == 5 ]"),
            printTest(ListOfValues(TextValue(), ListConstraint.Unique), "text* [ unique ]")
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
        suite("Round-trip tests")(
            roundTripTest(AnyValue(), "any"),
            roundTripTest(TextValue(), "text"),
            roundTripTest(NumericValue(), "number"),
            roundTripTest(BooleanValue(), "boolean"),
            roundTripTest(BinaryValue(), "binary"),
            roundTripTest(TextValue(textLength >= 10, textLength <= 100), "text with length constraints"),
            roundTripTest(NumericValue(NumericConstraint.Integer, numValue >= 0), "integer with min value"),
            roundTripTest(ListOfValues(TextValue()), "list of text"),
            roundTripTest(ListOfValues(NumericValue(), listSize >= 1), "non-empty list"),
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
            roundTripTest(TupleValue(TextValue(), NumericValue(), BooleanValue()), "tuple")
        )
    )

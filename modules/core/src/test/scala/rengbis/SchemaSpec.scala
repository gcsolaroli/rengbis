package rengbis

import zio.test.{ assertTrue, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses

import java.nio.file.{ Files, Path, Paths }
import scala.jdk.CollectionConverters.IteratorHasAsScala
import rengbis.Schema.{ AlternativeValues, AnyValue, BinaryValue, EnumValues, ListOfValues, MandatoryLabel, NumericValue, ObjectValue, Schema, TextValue, TupleValue }
import rengbis.Schema.{ BinaryConstraint, ListConstraint, NumericConstraint, TextConstraint }
import rengbis.testHelpers.{ binBytes, listSize, numValue, textLength }

object SchemaSpec extends ZIOSpecDefault:
    def parse(text: String): Either[String, Schema]    = SchemaLoader.parseSchema(text).map(s => s.root.get)
    def parseTest(text: String, expectedValue: Schema) = test(text) { assertTrue(parse(text) == Right(expectedValue)) }

    def spec = suite("Schema parsing features")(
        parseTest("= any", AnyValue()),
        parseTest("= number", NumericValue()),
        parseTest("= text", TextValue()),
        parseTest("= number*", ListOfValues(NumericValue())),
        parseTest("= number+", ListOfValues(NumericValue(), listSize >= 1)),
        parseTest("= text | number", AlternativeValues(TextValue(), NumericValue())),
        parseTest("= text | number*", AlternativeValues(TextValue(), ListOfValues(NumericValue()))),
        parseTest("= text* | number", AlternativeValues(ListOfValues(TextValue()), NumericValue())),
        parseTest("= (text | number)*", ListOfValues(AlternativeValues(TextValue(), NumericValue()))),
        parseTest("""= "yes" | "no" """, EnumValues("yes", "no")),
        //
        parseTest("""= text [ length == 10 ]""", TextValue(textLength === 10)),
        parseTest("""= text [ 10 <= length <= 100 ]""", TextValue(textLength >= 10, textLength <= 100)),
        parseTest("""= text [ 10 < length < 100 ]""", TextValue(textLength > 10, textLength < 100)),
        parseTest("""= text [ 10 < length <= 100 ]""", TextValue(textLength > 10, textLength <= 100)),
        parseTest("""= text [ 10 <= length < 100 ]""", TextValue(textLength >= 10, textLength < 100)),
        parseTest("""= text [ 10 <= length ]""", TextValue(textLength >= 10)),
        parseTest("""= text [ 10 < length ]""", TextValue(textLength > 10)),
        parseTest("""= text [ length >= 10 ]""", TextValue(textLength >= 10)),
        parseTest("""= text [ length > 10 ]""", TextValue(textLength > 10)),
        parseTest("""= text [ length <= 100 ]""", TextValue(textLength <= 100)),
        parseTest("""= text [ length < 100 ]""", TextValue(textLength < 100)),
        //
        parseTest("""= number [ integer ]""", NumericValue(NumericConstraint.Integer)),
        parseTest("""= number [ value >= 0 ]""", NumericValue(numValue >= 0)),
        parseTest("""= number [ value > 0 ]""", NumericValue(numValue > 0)),
        parseTest("""= number [ value <= 100 ]""", NumericValue(numValue <= 100)),
        parseTest("""= number [ value < 100 ]""", NumericValue(numValue < 100)),
        parseTest("""= number [ value == 42 ]""", NumericValue(numValue === 42)),
        parseTest("""= number [ 0 <= value <= 100 ]""", NumericValue(numValue >= 0, numValue <= 100)),
        parseTest("""= number [ 0 < value < 100 ]""", NumericValue(numValue > 0, numValue < 100)),
        parseTest("""= number [ 0 < value <= 100 ]""", NumericValue(numValue > 0, numValue <= 100)),
        parseTest("""= number [ 0 <= value < 100 ]""", NumericValue(numValue >= 0, numValue < 100)),
        parseTest("""= number [ integer, value >= 0 ]""", NumericValue(NumericConstraint.Integer, numValue >= 0)),
        parseTest("""= number [ integer, 1 <= value <= 12 ]""", NumericValue(NumericConstraint.Integer, numValue >= 1, numValue <= 12)),
        parseTest("""= number [ value >= -10 ]""", NumericValue(numValue >= -10)),
        parseTest("""= number [ value >= 0.5 ]""", NumericValue(numValue >= BigDecimal("0.5"))),
        //
        parseTest("= binary", BinaryValue()),
        parseTest("""= binary [ encoding = 'base64' ]""", BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.base64))),
        parseTest("""= binary [ encoding = 'hex' ]""", BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.hex))),
        parseTest("""= binary [ bytes == 32 ]""", BinaryValue(binBytes === 32)),
        parseTest("""= binary [ bytes >= 16 ]""", BinaryValue(binBytes >= 16)),
        parseTest("""= binary [ bytes <= 256 ]""", BinaryValue(binBytes <= 256)),
        parseTest("""= binary [ 16 <= bytes <= 64 ]""", BinaryValue(binBytes >= 16, binBytes <= 64)),
        parseTest("""= binary [ 16 <= KB <= 64 ]""", BinaryValue(binBytes >= 16384, binBytes <= 65536)),
        parseTest(
            """= binary [ encoding = 'base64', bytes == 32 ]""",
            BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.base64), binBytes === 32)
        ),
        parseTest(
            """= binary [ encoding = 'hex', 8 <= bytes <= 64 ]""",
            BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.hex), binBytes >= 8, binBytes <= 64)
        ),
        //
        parseTest("""= text [ 10 <= length <= 100 ]*""", ListOfValues(TextValue(textLength >= 10, textLength <= 100))),
        parseTest("""= text* [ size == 10 ]""", ListOfValues(TextValue(), listSize === 10)),
        parseTest("""= text [ 10 <= length <= 100 ]* [ size == 10 ]""", ListOfValues(TextValue(textLength >= 10, textLength <= 100), listSize === 10)),
        parseTest("""= text* [ unique ]""", ListOfValues(TextValue(), ListConstraint.Unique)),
        parseTest("""= number+ [ unique ]""", ListOfValues(NumericValue(), listSize >= 1, ListConstraint.Unique)),
        parseTest("""= text* [ unique, size == 3 ]""", ListOfValues(TextValue(), ListConstraint.Unique, listSize === 3)),
        parseTest("""= text* [ unique, 2 <= size <= 5 ]""", ListOfValues(TextValue(), ListConstraint.Unique, listSize >= 2, listSize <= 5)),
        parseTest(
            """= { id: text }* [ unique = id ]""",
            ListOfValues(ObjectValue(Map(MandatoryLabel("id") -> TextValue())), ListConstraint.UniqueByFields(Seq("id")))
        ),
        parseTest(
            """= { id: text, name: text }* [ unique = (id, name) ]""",
            ListOfValues(
                ObjectValue(Map(MandatoryLabel("id") -> TextValue(), MandatoryLabel("name") -> TextValue())),
                ListConstraint.UniqueByFields(Seq("id", "name"))
            )
        ),
        parseTest(
            """= { id: text, code: text }* [ unique = id, unique = code ]""",
            ListOfValues(
                ObjectValue(Map(MandatoryLabel("id") -> TextValue(), MandatoryLabel("code") -> TextValue())),
                ListConstraint.UniqueByFields(Seq("id")),
                ListConstraint.UniqueByFields(Seq("code"))
            )
        ),
        parseTest("""= text* [ size == 5 ]""", ListOfValues(TextValue(), listSize === 5)),
        parseTest("""= text* [ size >= 2 ]""", ListOfValues(TextValue(), listSize >= 2)),
        parseTest("""= text* [ size > 2 ]""", ListOfValues(TextValue(), listSize > 2)),
        parseTest("""= text* [ size <= 10 ]""", ListOfValues(TextValue(), listSize <= 10)),
        parseTest("""= text* [ size < 10 ]""", ListOfValues(TextValue(), listSize < 10)),
        parseTest("""= text* [ 2 <= size <= 5 ]""", ListOfValues(TextValue(), listSize >= 2, listSize <= 5)),
        parseTest("""= text* [ 2 < size < 10 ]""", ListOfValues(TextValue(), listSize > 2, listSize < 10)),
        parseTest("""= text* [ 2 <= size ]""", ListOfValues(TextValue(), listSize >= 2)),
        parseTest("""= text* [ unique, 2 <= size <= 5 ]""", ListOfValues(TextValue(), ListConstraint.Unique, listSize >= 2, listSize <= 5)),
        // ----------------------------------------------------------------
        test("with extra empty lines"):
            val schemaDefinition = """

= text | number


"""
            assertTrue(parse(schemaDefinition) == Right(AlternativeValues(TextValue(), NumericValue())))
        , // ----------------------------------------------------------------
        test("simple JSON/Yaml structure"):
            val schemaDefinition = """= {
	name: text,
	age: number,
	hobbies: text*
}"""
            val expectedSchema   = ObjectValue(
                Map(
                    MandatoryLabel("name")    -> TextValue(),
                    MandatoryLabel("age")     -> NumericValue(),
                    MandatoryLabel("hobbies") -> ListOfValues(TextValue())
                )
            )

            assertTrue(parse(schemaDefinition) == Right(expectedSchema))
        , // ----------------------------------------------------------------
        test("named value"):
            val schemaDefinition = """
foo = number*
= foo
"""
            assertTrue(parse(schemaDefinition) == Right(ListOfValues(NumericValue())))
        , // ----------------------------------------------------------------
        test("named values"):
            val schemaDefinition = """
foo = number*
bar = text*
= foo | bar
"""
            assertTrue(parse(schemaDefinition) == Right(AlternativeValues(ListOfValues(NumericValue()), ListOfValues(TextValue()))))
        , // ----------------------------------------------------------------
        test("named values inside object"):
            val schemaDefinition = """
foo = number*
bar = text*
= {
    foo: foo,
    bar: bar
}
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    ObjectValue(
                        Map(
                            (MandatoryLabel("foo") -> ListOfValues(NumericValue())),
                            (MandatoryLabel("bar") -> ListOfValues(TextValue()))
                        )
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("nested named values"):
            val schemaDefinition = """
foo = number*
bar = text* | foo

= bar
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    AlternativeValues(
                        ListOfValues(TextValue()),
                        ListOfValues(NumericValue())
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("named value object"):
            val schemaDefinition = """
bar = {
    bar_key_1: text*,
    bar_key_2: number*
}

= {
    key_1: number,
    key_2: bar
}
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    ObjectValue(
                        Map(
                            (MandatoryLabel("key_1") -> NumericValue()),
                            (MandatoryLabel("key_2") -> ObjectValue(
                                Map(
                                    (MandatoryLabel("bar_key_1") -> ListOfValues(TextValue())),
                                    (MandatoryLabel("bar_key_2") -> ListOfValues(NumericValue()))
                                )
                            ))
                        )
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("nested named value object"):
            val schemaDefinition = """
foo = {
    foo_key_1: number,
    foo_key_2: text
}

bar = {
    bar_key_1: text*,
    bar_key_2: foo*
}

= {
    key_1: number,
    key_2: bar
}
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    ObjectValue(
                        Map(
                            (MandatoryLabel("key_1") -> NumericValue()),
                            (MandatoryLabel("key_2") -> ObjectValue(
                                Map(
                                    (MandatoryLabel("bar_key_1") -> ListOfValues(TextValue())),
                                    (MandatoryLabel("bar_key_2") -> ListOfValues(
                                        ObjectValue(
                                            (Map(
                                                (MandatoryLabel("foo_key_1") -> NumericValue()),
                                                (MandatoryLabel("foo_key_2") -> TextValue())
                                            ))
                                        )
                                    ))
                                )
                            ))
                        )
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("simple tuple value"):
            val schemaDefinition = """
= (text, text, number)
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    TupleValue(
                        TextValue(),
                        TextValue(),
                        NumericValue()
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("tuple value"):
            val schemaDefinition = """
= (text [ 10 <= length <= 100 ], text [ length <= 10 ], number)
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    TupleValue(
                        TextValue(textLength >= 10, textLength <= 100),
                        TextValue(textLength <= 10),
                        NumericValue()
                    )
                )
            )
        , // ----------------------------------------------------------------
        test("alternative values lined up on multiple lines"):
            val schemaDefinition = """
foo = text
    | number
= foo
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    AlternativeValues(TextValue(), NumericValue())
                )
            )
        // ----------------------------------------------------------------
    )

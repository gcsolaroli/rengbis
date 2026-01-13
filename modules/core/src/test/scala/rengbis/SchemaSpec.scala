package rengbis

import zio.test.{ assertTrue, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses

import java.nio.file.{ Files, Path, Paths }
import scala.jdk.CollectionConverters.IteratorHasAsScala
import rengbis.Schema.{ AlternativeValues, AnyValue, BinaryValue, EnumValues, ListOfValues, MandatoryLabel, NumericValue, ObjectValue, Schema, TextValue, TupleValue }
import rengbis.Schema.{ BinaryConstraint, ListConstraint, NumericConstraint, TextConstraint }

object SchemaSpec extends ZIOSpecDefault:
    def parse(text: String): Either[String, Schema]    = SchemaLoader.parseSchema(text).map(s => s.root.get)
    def parseTest(text: String, expectedValue: Schema) = test(text) { assertTrue(parse(text) == Right(expectedValue)) }

    def spec = suite("Schema parsing features")(
        parseTest("= any", AnyValue()),
        parseTest("= number", NumericValue()),
        parseTest("= text", TextValue()),
        parseTest("= number*", ListOfValues(NumericValue())),
        parseTest("= number+", ListOfValues(NumericValue(), ListConstraint.MinSize(1))),
        parseTest("= text | number", AlternativeValues(TextValue(), NumericValue())),
        parseTest("= text | number*", AlternativeValues(TextValue(), ListOfValues(NumericValue()))),
        parseTest("= text* | number", AlternativeValues(ListOfValues(TextValue()), NumericValue())),
        parseTest("= (text | number)*", ListOfValues(AlternativeValues(TextValue(), NumericValue()))),
        parseTest("""= "yes" | "no" """, EnumValues("yes", "no")),
        //
        parseTest("""= text [ length == 10 ]""", TextValue(TextConstraint.Length(10))),
        parseTest("""= text [ 10 <= length <= 100 ]""", TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100))),
        parseTest("""= text [ 10 < length < 100 ]""", TextValue(TextConstraint.MinLength(11), TextConstraint.MaxLength(99))),
        parseTest("""= text [ 10 < length <= 100 ]""", TextValue(TextConstraint.MinLength(11), TextConstraint.MaxLength(100))),
        parseTest("""= text [ 10 <= length < 100 ]""", TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(99))),
        parseTest("""= text [ 10 <= length ]""", TextValue(TextConstraint.MinLength(10))),
        parseTest("""= text [ 10 < length ]""", TextValue(TextConstraint.MinLength(11))),
        parseTest("""= text [ length >= 10 ]""", TextValue(TextConstraint.MinLength(10))),
        parseTest("""= text [ length > 10 ]""", TextValue(TextConstraint.MinLength(11))),
        parseTest("""= text [ length <= 100 ]""", TextValue(TextConstraint.MaxLength(100))),
        parseTest("""= text [ length < 100 ]""", TextValue(TextConstraint.MaxLength(99))),
        //
        parseTest("""= number [ integer ]""", NumericValue(NumericConstraint.Integer)),
        parseTest("""= number [ value >= 0 ]""", NumericValue(NumericConstraint.MinValue(0))),
        parseTest("""= number [ value > 0 ]""", NumericValue(NumericConstraint.MinValueExclusive(0))),
        parseTest("""= number [ value <= 100 ]""", NumericValue(NumericConstraint.MaxValue(100))),
        parseTest("""= number [ value < 100 ]""", NumericValue(NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number [ value == 42 ]""", NumericValue(NumericConstraint.ExactValue(42))),
        parseTest("""= number [ 0 <= value <= 100 ]""", NumericValue(NumericConstraint.MinValue(0), NumericConstraint.MaxValue(100))),
        parseTest("""= number [ 0 < value < 100 ]""", NumericValue(NumericConstraint.MinValueExclusive(0), NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number [ 0 < value <= 100 ]""", NumericValue(NumericConstraint.MinValueExclusive(0), NumericConstraint.MaxValue(100))),
        parseTest("""= number [ 0 <= value < 100 ]""", NumericValue(NumericConstraint.MinValue(0), NumericConstraint.MaxValueExclusive(100))),
        parseTest("""= number [ integer, value >= 0 ]""", NumericValue(NumericConstraint.Integer, NumericConstraint.MinValue(0))),
        parseTest("""= number [ integer, 1 <= value <= 12 ]""", NumericValue(NumericConstraint.Integer, NumericConstraint.MinValue(1), NumericConstraint.MaxValue(12))),
        parseTest("""= number [ value >= -10 ]""", NumericValue(NumericConstraint.MinValue(-10))),
        parseTest("""= number [ value >= 0.5 ]""", NumericValue(NumericConstraint.MinValue(BigDecimal("0.5")))),
        //
        parseTest("= binary", BinaryValue()),
        parseTest("""= binary [ encoding = 'base64' ]""", BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.base64))),
        parseTest("""= binary [ encoding = 'hex' ]""", BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.hex))),
        parseTest("""= binary [ bytes == 32 ]""", BinaryValue(BinaryConstraint.ExactSize(32, BinaryConstraint.BinaryUnit.bytes))),
        parseTest("""= binary [ bytes >= 16 ]""", BinaryValue(BinaryConstraint.MinSize(16, BinaryConstraint.BinaryUnit.bytes))),
        parseTest("""= binary [ bytes <= 256 ]""", BinaryValue(BinaryConstraint.MaxSize(256, BinaryConstraint.BinaryUnit.bytes))),
        parseTest(
            """= binary [ 16 <= bytes <= 64 ]""",
            BinaryValue(BinaryConstraint.MinSize(16, BinaryConstraint.BinaryUnit.bytes), BinaryConstraint.MaxSize(64, BinaryConstraint.BinaryUnit.bytes))
        ),
        parseTest(
            """= binary [ encoding = 'base64', bytes == 32 ]""",
            BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.base64), BinaryConstraint.ExactSize(32, BinaryConstraint.BinaryUnit.bytes))
        ),
        parseTest(
            """= binary [ encoding = 'hex', 8 <= bytes <= 64 ]""",
            BinaryValue(
                BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.hex),
                BinaryConstraint.MinSize(8, BinaryConstraint.BinaryUnit.bytes),
                BinaryConstraint.MaxSize(64, BinaryConstraint.BinaryUnit.bytes)
            )
        ),
        //
        parseTest("""= text [ 10 <= length <= 100 ]*""", ListOfValues(TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)))),
        parseTest("""= text* [ size == 10 ]""", ListOfValues(TextValue(), ListConstraint.ExactSize(10))),
        parseTest(
            """= text [ 10 <= length <= 100 ]* [ size == 10 ]""",
            ListOfValues(TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)), ListConstraint.ExactSize(10))
        ),
        parseTest("""= text* [ unique ]""", ListOfValues(TextValue(), ListConstraint.Unique)),
        parseTest("""= number+ [ unique ]""", ListOfValues(NumericValue(), ListConstraint.MinSize(1), ListConstraint.Unique)),
        parseTest("""= text* [ unique, size == 3 ]""", ListOfValues(TextValue(), ListConstraint.Unique, ListConstraint.ExactSize(3))),
        parseTest("""= text* [ unique, 2 <= size <= 5 ]""", ListOfValues(TextValue(), ListConstraint.Unique, ListConstraint.MinSize(2), ListConstraint.MaxSize(5))),
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
        parseTest("""= text* [ size == 5 ]""", ListOfValues(TextValue(), ListConstraint.ExactSize(5))),
        parseTest("""= text* [ size >= 2 ]""", ListOfValues(TextValue(), ListConstraint.MinSize(2))),
        parseTest("""= text* [ size > 2 ]""", ListOfValues(TextValue(), ListConstraint.MinSize(3))),
        parseTest("""= text* [ size <= 10 ]""", ListOfValues(TextValue(), ListConstraint.MaxSize(10))),
        parseTest("""= text* [ size < 10 ]""", ListOfValues(TextValue(), ListConstraint.MaxSize(9))),
        parseTest("""= text* [ 2 <= size <= 5 ]""", ListOfValues(TextValue(), ListConstraint.MinSize(2), ListConstraint.MaxSize(5))),
        parseTest("""= text* [ 2 < size < 10 ]""", ListOfValues(TextValue(), ListConstraint.MinSize(3), ListConstraint.MaxSize(9))),
        parseTest("""= text* [ 2 <= size ]""", ListOfValues(TextValue(), ListConstraint.MinSize(2))),
        parseTest(
            """= text* [ unique, 2 <= size <= 5 ]""",
            ListOfValues(TextValue(), ListConstraint.Unique, ListConstraint.MinSize(2), ListConstraint.MaxSize(5))
        ), // ----------------------------------------------------------------
        test("with extra empty lines"):
            val schemaDefinition = """

= text | number


"""
            assertTrue(parse(schemaDefinition) == Right(AlternativeValues(TextValue(), NumericValue())))
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
        test("named value"):
            val schemaDefinition = """
foo = number*
= foo
"""
            assertTrue(parse(schemaDefinition) == Right(ListOfValues(NumericValue())))
        ,  // ----------------------------------------------------------------
        test("named values"):
            val schemaDefinition = """
foo = number*
bar = text*
= foo | bar
"""
            assertTrue(parse(schemaDefinition) == Right(AlternativeValues(ListOfValues(NumericValue()), ListOfValues(TextValue()))))
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
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
        ,  // ----------------------------------------------------------------
        test("tuple value"):
            val schemaDefinition = """
= (text [ 10 <= length <= 100 ], text [ length <= 10 ], number)
"""
            assertTrue(
                parse(schemaDefinition) == Right(
                    TupleValue(
                        TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)),
                        TextValue(TextConstraint.MaxLength(10)),
                        NumericValue()
                    )
                )
            )
        ,  // ----------------------------------------------------------------
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

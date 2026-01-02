package rengbis

import zio.test.{ assertTrue, TestConsole, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses
import zio.Chunk
import rengbis.Schema.*

object SchemaSpec extends ZIOSpecDefault:
    def parseTest(text: String, expectedValue: Schema) = test(text) { assertTrue(parse(text) == Right(expectedValue)) }

    def spec = suite("Schema parsing features")(
        parseTest("= number", NumericValue()),
        parseTest("= text", TextValue()),
        parseTest("= number*", ListOfValues(NumericValue())),
        parseTest("= number+", ListOfValues(NumericValue(), ListConstraint.MinSize(1))),
        parseTest("= text | number", AlternativeValues(TextValue(), NumericValue())),
        parseTest("= text | number*", AlternativeValues(TextValue(), ListOfValues(NumericValue()))),
        parseTest("= text* | number", AlternativeValues(ListOfValues(TextValue()), NumericValue())),
        parseTest("= (text | number)*", ListOfValues(AlternativeValues(TextValue(), NumericValue()))),
        parseTest("""= "yes" | "no" """, EnumValues("yes", "no")),
        parseTest("""= text { length == 10 }""", TextValue(TextConstraint.Length(10))),
        parseTest("""= text { 10 <= length <= 100 }""", TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100))),
        parseTest("""= text { 10 < length < 100 }""", TextValue(TextConstraint.MinLength(11), TextConstraint.MaxLength(99))),
        parseTest("""= text { 10 < length <= 100 }""", TextValue(TextConstraint.MinLength(11), TextConstraint.MaxLength(100))),
        parseTest("""= text { 10 <= length < 100 }""", TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(99))),
        parseTest("""= text { 10 <= length }""", TextValue(TextConstraint.MinLength(10))),
        parseTest("""= text { 10 < length }""", TextValue(TextConstraint.MinLength(11))),
        parseTest("""= text { length >= 10 }""", TextValue(TextConstraint.MinLength(10))),
        parseTest("""= text { length > 10 }""", TextValue(TextConstraint.MinLength(11))),
        parseTest("""= text { length <= 100 }""", TextValue(TextConstraint.MaxLength(100))),
        parseTest("""= text { length < 100 }""", TextValue(TextConstraint.MaxLength(99))),

        // parseTest("""= text { regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})" }""", TextValue(TextConstraint.Regex("([0-9]{4}-[0-9]{2}-[0-9]{2})".r))),

        parseTest("""= text { 10 <= length <= 100 }*""", ListOfValues(TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)))),
        parseTest("""= text{10}""", ListOfValues(TextValue(), ListConstraint.ExactSize(10))),
        parseTest(
            """= text { 10 <= length <= 100 }{10}""",
            ListOfValues(TextValue(TextConstraint.MinLength(10), TextConstraint.MaxLength(100)), ListConstraint.ExactSize(10))
        ),

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
= (text { 10 <= length <= 100 }, text { length <= 10 }, number)
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
        , // ----------------------------------------------------------------
        test("degenerated tuple value"):
            val schemaDefinition = """
= (text)
"""
            allSuccesses(
                assertTrue(parse(schemaDefinition).isLeft),
                assertTrue(parse(schemaDefinition).swap.getOrElse("").contains("tuple needs to have at least two items"))
            )
        // ----------------------------------------------------------------
    )

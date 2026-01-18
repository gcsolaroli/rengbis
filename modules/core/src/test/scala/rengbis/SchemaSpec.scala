package rengbis

import zio.test.{ assertTrue, ZIOSpecDefault }

import rengbis.Schema.{ AlternativeValues, AnyValue, BinaryValue, Deprecated, Documented, EnumValues, ListOfValues, MandatoryLabel, NumericValue, ObjectValue, Schema, TextValue, TimeValue, TupleValue }
import rengbis.Schema.{ BinaryConstraint, ListConstraint, NumericConstraint, TextConstraint, TimeConstraint }
import rengbis.testHelpers.{ binBytes, listSize, numValue, textLength }

object SchemaSpec extends ZIOSpecDefault:
    def parse(text: String): Either[String, Schema]    = SchemaLoader.parseSchema(text).map(s => s.root.get)
    def parseTest(text: String, expectedValue: Schema) = test(text) { assertTrue(parse(text) == Right(expectedValue)) }

    // Helper methods to create constrained values
    def textWithSize(size: TextConstraint.SizeRange): TextValue =
        TextValue(TextConstraint.Constraints(size = Some(size)))

    def numWithValue(value: NumericConstraint.ValueRange): NumericValue =
        NumericValue(NumericConstraint.Constraints(value = Some(value)))

    def numInteger: NumericValue =
        NumericValue(NumericConstraint.Constraints(integer = true))

    def numIntegerWithValue(value: NumericConstraint.ValueRange): NumericValue =
        NumericValue(NumericConstraint.Constraints(value = Some(value), integer = true))

    def listWithSize(schema: Schema, size: ListConstraint.SizeRange): ListOfValues =
        ListOfValues(schema, ListConstraint.Constraints(size = Some(size)))

    def listWithUnique(schema: Schema): ListOfValues =
        ListOfValues(schema, ListConstraint.Constraints(unique = Seq(ListConstraint.Uniqueness.Simple)))

    def listWithSizeAndUnique(schema: Schema, size: ListConstraint.SizeRange): ListOfValues =
        ListOfValues(schema, ListConstraint.Constraints(size = Some(size), unique = Seq(ListConstraint.Uniqueness.Simple)))

    def listWithUniqueByFields(schema: Schema, fields: Seq[String]): ListOfValues =
        ListOfValues(schema, ListConstraint.Constraints(unique = Seq(ListConstraint.Uniqueness.ByFields(fields))))

    def binWithSize(size: BinaryConstraint.SizeRange): BinaryValue =
        BinaryValue(BinaryConstraint.Constraints(size = Some(size)))

    def binWithEncoding(enc: BinaryConstraint.BinaryToTextEncoder): BinaryValue =
        BinaryValue(BinaryConstraint.Constraints(encoding = Some(enc)))

    def binWithEncodingAndSize(enc: BinaryConstraint.BinaryToTextEncoder, size: BinaryConstraint.SizeRange): BinaryValue =
        BinaryValue(BinaryConstraint.Constraints(encoding = Some(enc), size = Some(size)))

    def spec = suite("Schema parsing features")(
        suite("Basic types")(
            parseTest("= any", AnyValue()),
            parseTest("= number", NumericValue()),
            parseTest("= text", TextValue()),
            parseTest("= binary", BinaryValue()),
            parseTest("= time [ format = 'iso8601' ]", TimeValue(TimeConstraint.NamedFormat.ISO8601)),
            parseTest("= time [ format = 'iso8601-date' ]", TimeValue(TimeConstraint.NamedFormat.ISO8601_Date)),
            parseTest("= time [ format = 'iso8601-time' ]", TimeValue(TimeConstraint.NamedFormat.ISO8601_Time)),
            parseTest("""= time [ format = "yyyy-MM-dd" ]""", TimeValue(TimeConstraint.CustomPattern("yyyy-MM-dd"))),
            test("invalid time pattern should be rejected"):
                val result = parse("""= time [ format = "INVALID" ]""")
                assertTrue(result.isLeft)
        ),
        suite("List types")(
            parseTest("= number*", ListOfValues(NumericValue())),
            parseTest("= number+", listWithSize(NumericValue(), listSize >= 1)),
            parseTest("= text*", ListOfValues(TextValue())),
            parseTest("""= text [ 10 <= length <= 100 ]*""", ListOfValues(textWithSize((textLength >= 10).merge(textLength <= 100))))
        ),
        suite("Alternative types")(
            parseTest("= text | number", AlternativeValues(TextValue(), NumericValue())),
            parseTest("= text | number*", AlternativeValues(TextValue(), ListOfValues(NumericValue()))),
            parseTest("= text* | number", AlternativeValues(ListOfValues(TextValue()), NumericValue())),
            parseTest("= (text | number)*", ListOfValues(AlternativeValues(TextValue(), NumericValue()))),
            test("alternative values lined up on multiple lines"):
                val schemaDefinition = """
                    |foo = text
                    |    | number
                    |= foo
                    |""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        AlternativeValues(TextValue(), NumericValue())
                    )
                )
        ),
        suite("Enum values")(
            parseTest("""= "yes" | "no" """, EnumValues("yes", "no"))
        ),
        suite("Text constraints")(
            parseTest("""= text [ length == 10 ]""", textWithSize(textLength === 10)),
            parseTest("""= text [ 10 <= length <= 100 ]""", textWithSize((textLength >= 10).merge(textLength <= 100))),
            parseTest("""= text [ 10 < length < 100 ]""", textWithSize((textLength > 10).merge(textLength < 100))),
            parseTest("""= text [ 10 < length <= 100 ]""", textWithSize((textLength > 10).merge(textLength <= 100))),
            parseTest("""= text [ 10 <= length < 100 ]""", textWithSize((textLength >= 10).merge(textLength < 100))),
            parseTest("""= text [ 10 <= length ]""", textWithSize(textLength >= 10)),
            parseTest("""= text [ 10 < length ]""", textWithSize(textLength > 10)),
            parseTest("""= text [ length >= 10 ]""", textWithSize(textLength >= 10)),
            parseTest("""= text [ length > 10 ]""", textWithSize(textLength > 10)),
            parseTest("""= text [ length <= 100 ]""", textWithSize(textLength <= 100)),
            parseTest("""= text [ length < 100 ]""", textWithSize(textLength < 100))
        ),
        suite("Number constraints")(
            parseTest("""= number [ integer ]""", numInteger),
            parseTest("""= number [ value >= 0 ]""", numWithValue(numValue >= 0)),
            parseTest("""= number [ value > 0 ]""", numWithValue(numValue > 0)),
            parseTest("""= number [ value <= 100 ]""", numWithValue(numValue <= 100)),
            parseTest("""= number [ value < 100 ]""", numWithValue(numValue < 100)),
            parseTest("""= number [ value == 42 ]""", numWithValue(numValue === 42)),
            parseTest("""= number [ 0 <= value <= 100 ]""", numWithValue((numValue >= 0).merge(numValue <= 100))),
            parseTest("""= number [ 0 < value < 100 ]""", numWithValue((numValue > 0).merge(numValue < 100))),
            parseTest("""= number [ 0 < value <= 100 ]""", numWithValue((numValue > 0).merge(numValue <= 100))),
            parseTest("""= number [ 0 <= value < 100 ]""", numWithValue((numValue >= 0).merge(numValue < 100))),
            parseTest("""= number [ integer, value >= 0 ]""", numIntegerWithValue(numValue >= 0)),
            parseTest("""= number [ integer, 1 <= value <= 12 ]""", numIntegerWithValue((numValue >= 1).merge(numValue <= 12))),
            parseTest("""= number [ value >= -10 ]""", numWithValue(numValue >= -10)),
            parseTest("""= number [ value >= 0.5 ]""", numWithValue(numValue >= BigDecimal("0.5")))
        ),
        suite("Binary constraints")(
            parseTest("""= binary [ encoding = 'base64' ]""", binWithEncoding(BinaryConstraint.BinaryToTextEncoder.base64)),
            parseTest("""= binary [ encoding = 'hex' ]""", binWithEncoding(BinaryConstraint.BinaryToTextEncoder.hex)),
            parseTest("""= binary [ bytes == 32 ]""", binWithSize(binBytes === 32)),
            parseTest("""= binary [ bytes >= 16 ]""", binWithSize(binBytes >= 16)),
            parseTest("""= binary [ bytes <= 256 ]""", binWithSize(binBytes <= 256)),
            parseTest("""= binary [ 16 <= bytes <= 64 ]""", binWithSize((binBytes >= 16).merge(binBytes <= 64))),
            parseTest("""= binary [ 16 <= KB <= 64 ]""", binWithSize((binBytes >= 16384).merge(binBytes <= 65536))),
            parseTest(
                """= binary [ encoding = 'base64', bytes == 32 ]""",
                binWithEncodingAndSize(BinaryConstraint.BinaryToTextEncoder.base64, binBytes === 32)
            ),
            parseTest(
                """= binary [ encoding = 'hex', 8 <= bytes <= 64 ]""",
                binWithEncodingAndSize(BinaryConstraint.BinaryToTextEncoder.hex, (binBytes >= 8).merge(binBytes <= 64))
            )
        ),
        suite("List constraints")(
            parseTest("""= text* [ size == 10 ]""", listWithSize(TextValue(), listSize === 10)),
            parseTest("""= text [ 10 <= length <= 100 ]* [ size == 10 ]""", listWithSize(textWithSize((textLength >= 10).merge(textLength <= 100)), listSize === 10)),
            parseTest("""= text* [ size == 5 ]""", listWithSize(TextValue(), listSize === 5)),
            parseTest("""= text* [ size >= 2 ]""", listWithSize(TextValue(), listSize >= 2)),
            parseTest("""= text* [ size > 2 ]""", listWithSize(TextValue(), listSize > 2)),
            parseTest("""= text* [ size <= 10 ]""", listWithSize(TextValue(), listSize <= 10)),
            parseTest("""= text* [ size < 10 ]""", listWithSize(TextValue(), listSize < 10)),
            parseTest("""= text* [ 2 <= size <= 5 ]""", listWithSize(TextValue(), (listSize >= 2).merge(listSize <= 5))),
            parseTest("""= text* [ 2 < size < 10 ]""", listWithSize(TextValue(), (listSize > 2).merge(listSize < 10))),
            parseTest("""= text* [ 2 <= size ]""", listWithSize(TextValue(), listSize >= 2))
        ),
        suite("List uniqueness constraints")(
            parseTest("""= text* [ unique ]""", listWithUnique(TextValue())),
            parseTest("""= number+ [ unique ]""", listWithSizeAndUnique(NumericValue(), listSize >= 1)),
            parseTest("""= text* [ unique, size == 3 ]""", listWithSizeAndUnique(TextValue(), listSize === 3)),
            parseTest("""= text* [ unique, 2 <= size <= 5 ]""", listWithSizeAndUnique(TextValue(), (listSize >= 2).merge(listSize <= 5))),
            parseTest(
                """= { id: text }* [ unique = id ]""",
                listWithUniqueByFields(ObjectValue(Map(MandatoryLabel("id") -> TextValue())), Seq("id"))
            ),
            parseTest(
                """= { id: text, name: text }* [ unique = (id, name) ]""",
                listWithUniqueByFields(
                    ObjectValue(Map(MandatoryLabel("id") -> TextValue(), MandatoryLabel("name") -> TextValue())),
                    Seq("id", "name")
                )
            ),
            parseTest(
                """= { id: text, code: text }* [ unique = id, unique = code ]""",
                ListOfValues(
                    ObjectValue(Map(MandatoryLabel("id") -> TextValue(), MandatoryLabel("code") -> TextValue())),
                    ListConstraint.Constraints(unique = Seq(ListConstraint.Uniqueness.ByFields(Seq("id")), ListConstraint.Uniqueness.ByFields(Seq("code"))))
                )
            ),
            parseTest("""= text* [ unique, 2 <= size <= 5 ]""", listWithSizeAndUnique(TextValue(), (listSize >= 2).merge(listSize <= 5)))
        ),
        suite("Object types")(
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
        ),
        suite("Named values")(
            test("named value"):
                val schemaDefinition = """
                    |foo = number*
                    |= foo
                    |""".stripMargin
                assertTrue(parse(schemaDefinition) == Right(ListOfValues(NumericValue())))
            ,
            test("named values"):
                val schemaDefinition = """
                    |foo = number*
                    |bar = text*
                    |= foo | bar
                    |""".stripMargin
                assertTrue(parse(schemaDefinition) == Right(AlternativeValues(ListOfValues(NumericValue()), ListOfValues(TextValue()))))
            ,
            test("named values inside object"):
                val schemaDefinition = """
                    |foo = number*
                    |bar = text*
                    |= {
                    |    foo: foo,
                    |    bar: bar
                    |}
                    |""".stripMargin
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
            ,
            test("nested named values"):
                val schemaDefinition = """
                    |foo = number*
                    |bar = text* | foo
                    |
                    |= bar
                    |""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        AlternativeValues(
                            ListOfValues(TextValue()),
                            ListOfValues(NumericValue())
                        )
                    )
                )
            ,
            test("named value object"):
                val schemaDefinition = """
                    |bar = {
                    |    bar_key_1: text*,
                    |    bar_key_2: number*
                    |}
                    |
                    |= {
                    |    key_1: number,
                    |    key_2: bar
                    |}
                    |""".stripMargin
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
            ,
            test("nested named value object"):
                val schemaDefinition = """
                    |foo = {
                    |    foo_key_1: number,
                    |    foo_key_2: text
                    |}
                    |
                    |bar = {
                    |    bar_key_1: text*,
                    |    bar_key_2: foo*
                    |}
                    |
                    |= {
                    |    key_1: number,
                    |    key_2: bar
                    |}
                    |""".stripMargin
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
        ),
        suite("Tuple types")(
            test("simple tuple value"):
                val schemaDefinition = "= (text, text, number)"
                assertTrue(
                    parse(schemaDefinition) == Right(
                        TupleValue(
                            TextValue(),
                            TextValue(),
                            NumericValue()
                        )
                    )
                )
            ,
            test("tuple value"):
                val schemaDefinition = "= (text [ 10 <= length <= 100 ], text [ length <= 10 ], number)"
                assertTrue(
                    parse(schemaDefinition) == Right(
                        TupleValue(
                            textWithSize((textLength >= 10).merge(textLength <= 100)),
                            textWithSize(textLength <= 10),
                            NumericValue()
                        )
                    )
                )
        ),
        suite("Default values")(
            test("text with default value"):
                val schemaDefinition = """= text ?= "active""""
                assertTrue(
                    parse(schemaDefinition) == Right(
                        TextValue(TextConstraint.Constraints(), Some("active"))
                    )
                )
            ,
            test("text with constraints and default value"):
                val schemaDefinition = """= text [ length <= 100 ] ?= "default""""
                assertTrue(
                    parse(schemaDefinition) == Right(
                        TextValue(TextConstraint.Constraints(size = Some(textLength <= 100)), Some("default"))
                    )
                )
            ,
            test("number with default value"):
                val schemaDefinition = """= number ?= 0"""
                assertTrue(
                    parse(schemaDefinition) == Right(
                        NumericValue(NumericConstraint.Constraints(), Some(BigDecimal(0)))
                    )
                )
            ,
            test("number with constraints and default value"):
                val schemaDefinition = """= number [ integer, value >= 0 ] ?= 42"""
                assertTrue(
                    parse(schemaDefinition) == Right(
                        NumericValue(NumericConstraint.Constraints(value = Some(numValue >= 0), integer = true), Some(BigDecimal(42)))
                    )
                )
            ,
            test("object with fields having default values"):
                val schemaDefinition = """= {
                    |    status: text ?= "active"
                    |    count: number [ integer ] ?= 0
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        ObjectValue(
                            Map(
                                MandatoryLabel("status") -> TextValue(TextConstraint.Constraints(), Some("active")),
                                MandatoryLabel("count")  -> NumericValue(NumericConstraint.Constraints(integer = true), Some(BigDecimal(0)))
                            )
                        )
                    )
                )
        ),
        suite("Whitespace handling")(
            test("with extra empty lines"):
                val schemaDefinition = """
                    |
                    |= text | number
                    |
                    |
                    |""".stripMargin
                assertTrue(parse(schemaDefinition) == Right(AlternativeValues(TextValue(), NumericValue())))
        ),
        suite("Documentation comments")(
            test("preceding doc comment on object field"):
                val schemaDefinition = """= {
                    |    ## This is the name field
                    |    name: text
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        ObjectValue(
                            Map(
                                MandatoryLabel("name") -> Documented(Some("This is the name field"), TextValue())
                            )
                        )
                    )
                )
            ,
            test("trailing doc comment on object field"):
                val schemaDefinition = """= {
                    |    name: text  ## This is the name field
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        ObjectValue(
                            Map(
                                MandatoryLabel("name") -> Documented(Some("This is the name field"), TextValue())
                            )
                        )
                    )
                )
            ,
            test("multi-line preceding doc comment"):
                val schemaDefinition = """= {
                    |    ## This is the name field
                    |    ## It contains the user's full name
                    |    name: text
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        ObjectValue(
                            Map(
                                MandatoryLabel("name") -> Documented(Some("This is the name field\nIt contains the user's full name"), TextValue())
                            )
                        )
                    )
                )
            ,
            test("multiple fields with doc comments"):
                val schemaDefinition = """= {
                    |    ## The user's name
                    |    name: text
                    |    ## The user's age
                    |    age: number
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        ObjectValue(
                            Map(
                                MandatoryLabel("name") -> Documented(Some("The user's name"), TextValue()),
                                MandatoryLabel("age")  -> Documented(Some("The user's age"), NumericValue())
                            )
                        )
                    )
                )
            ,
            test("mixed doc comments - some fields documented, some not"):
                val schemaDefinition = """
                    |= {
                    |    ## Documented field
                    |    name: text
                    |    age: number
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        ObjectValue(
                            Map(
                                MandatoryLabel("name") -> Documented(Some("Documented field"), TextValue()),
                                MandatoryLabel("age")  -> NumericValue()
                            )
                        )
                    )
                )
            ,
            test("doc comment on root schema"):
                val schemaDefinition = """
                    |## This is the root schema
                    |= text
                    |""".stripMargin
                assertTrue(parse(schemaDefinition) == Right(Documented(Some("This is the root schema"), TextValue())))
            ,
            test("doc comment on named value"):
                val schemaDefinition = """
                    |## Documentation for myType
                    |myType = number
                    |= myType""".stripMargin
                val result           = SchemaLoader.parseSchema(schemaDefinition)
                assertTrue(
                    result.isRight &&
                        result.toOption.get.definitions
                            .get("myType")
                            .contains(
                                Documented(Some("Documentation for myType"), NumericValue())
                            )
                )
            ,
            test("trailing doc comment on root schema"):
                val schemaDefinition = """= text  ## Root documentation"""
                assertTrue(
                    parse(schemaDefinition) == Right(
                        Documented(Some("Root documentation"), TextValue())
                    )
                )
            ,
            test("trailing doc comment on root object schema"):
                val schemaDefinition = """
                    |= {    ## Root documentation
                    |   name: text
                    |   value: number
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        Documented(
                            Some("Root documentation"),
                            ObjectValue(
                                Map(
                                    MandatoryLabel("name")  -> TextValue(),
                                    MandatoryLabel("value") -> NumericValue()
                                )
                            )
                        )
                    )
                )
            ,
            test("trailing doc comment on root object schema, take 2"):
                val schemaDefinition = """
                    |= {
                    |   name: text
                    |   value: number
                    |} ## Root documentation""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        Documented(
                            Some("Root documentation"),
                            ObjectValue(
                                Map(
                                    MandatoryLabel("name")  -> TextValue(),
                                    MandatoryLabel("value") -> NumericValue()
                                )
                            )
                        )
                    )
                )
            ,
            test("trailing doc comment on root object schema, take 3"):
                val schemaDefinition = """
                    |## Root documentation
                    |= {
                    |   name: text
                    |   value: number
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        Documented(
                            Some("Root documentation"),
                            ObjectValue(
                                Map(
                                    MandatoryLabel("name")  -> TextValue(),
                                    MandatoryLabel("value") -> NumericValue()
                                )
                            )
                        )
                    )
                )
        ),
        suite("Deprecated annotation")(
            test("@deprecated on object field"):
                val schemaDefinition = """= {
                    |    @deprecated oldField: text
                    |    newField: number
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        ObjectValue(
                            Map(
                                MandatoryLabel("oldField") -> Deprecated(TextValue()),
                                MandatoryLabel("newField") -> NumericValue()
                            )
                        )
                    )
                )
            ,
            test("@deprecated on named value"):
                val schemaDefinition = """@deprecated legacyType = text
                    |= legacyType""".stripMargin
                val result           = SchemaLoader.parseSchema(schemaDefinition)
                assertTrue(
                    result.isRight &&
                        result.toOption.get.definitions.get("legacyType").contains(Deprecated(TextValue()))
                )
            ,
            test("@deprecated with doc comment"):
                val schemaDefinition = """= {
                    |    ## This field is old
                    |    @deprecated oldField: text
                    |}""".stripMargin
                assertTrue(
                    parse(schemaDefinition) == Right(
                        ObjectValue(
                            Map(
                                MandatoryLabel("oldField") -> Deprecated(Documented(Some("This field is old"), TextValue()))
                            )
                        )
                    )
                )
        )
    )

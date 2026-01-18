package rengbis

import zio.test.{ assertTrue, TestConsole, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses
import zio.Chunk
import rengbis.Validator.ValidationResult

object ValidatorSpec extends ZIOSpecDefault:
    def stringValidator(parser: DataParsers.Parser[String])(schema: Schema.Schema, value: String): ValidationResult = Validator.validate(parser(value))(schema)

    val validateYamlString: (Schema.Schema, String) => ValidationResult = stringValidator(DataParsers.yaml)
    val validateJsonString: (Schema.Schema, String) => ValidationResult = stringValidator(DataParsers.json)
    val validateXmlString: (Schema.Schema, String) => ValidationResult  = stringValidator(DataParsers.xml)
    val validateString: (Schema.Schema, String) => ValidationResult     = stringValidator(DataParsers.text)

    def parse(text: String): Either[String, Schema.Schema] = SchemaLoader.parseSchema(text).map(s => s.root.get)

    def spec = suite("Validator")(
        suite("Any type")(
            test("accepts all values"):
                val schemaDefinition = """= any"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """true""").isValid),
                            assertTrue(validateJsonString(schema, """false""").isValid),
                            assertTrue(validateJsonString(schema, """"hello"""").isValid),
                            assertTrue(validateJsonString(schema, """42""").isValid),
                            assertTrue(validateJsonString(schema, """3.14""").isValid),
                            assertTrue(validateJsonString(schema, """null""").isValid),
                            assertTrue(validateJsonString(schema, """[]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", 123]""").isValid),
                            assertTrue(validateJsonString(schema, """{}""").isValid),
                            assertTrue(validateJsonString(schema, """{"key": "value", "num": 42}""").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("in object field"):
                val schemaDefinition = """= { name: text, data: any }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """{"name": "test", "data": true}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "test", "data": 42}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "test", "data": "hello"}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "test", "data": [1, 2, 3]}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "test", "data": {"nested": "object"}}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "test", "data": null}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": 41, "data": null}""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("in list"):
                val schemaDefinition = """= any*"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid),
                            assertTrue(validateJsonString(schema, """[1, "two", true, null, {"key": "value"}]""").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("Basic validation")(
            test("trivial Yaml values"):
                allSuccesses(parse("""= "yes" | "no" """) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateYamlString(schema, "yes").isValid),
                            assertTrue(validateYamlString(schema, "no").isValid),
                            assertTrue(validateYamlString(schema, "🤷‍♂️").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("simple JSON/Yaml/XML values"):
                val schemaDefinition = """= {
                                            name: text
                                            age: number
                                            hobbies: text*
                                        }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            //  JSON
                            assertTrue(validateJsonString(schema, """{"name": "John", "age":  30 ,     "hobbies": ["reading", "hiking"]}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "John", "age": "30",     "hobbies": ["reading", "hiking"]}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "John", "age": "thirty", "hobbies": ["reading", "hiking"]}""").isValid == false),

                            //  Yaml
                            assertTrue(validateYamlString(schema, "name: John\nage:   30      \nhobbies:\n  - reading\n  - hiking").isValid),
                            assertTrue(validateYamlString(schema, "name: John\nage: \"30\"    \nhobbies:\n  - reading\n  - hiking").isValid),
                            assertTrue(validateYamlString(schema, "name: John\nage: \"thirty\"\nhobbies:\n  - reading\n  - hiking").isValid == false),

                            //  XML
                            assertTrue(validateXmlString(schema, "<root><name>John</name><age>30    </age><hobbies>reading</hobbies><hobbies>hiking</hobbies></root>").isValid),
                            assertTrue(validateXmlString(schema, "<root><name>John</name><age>thirty</age><hobbies>reading</hobbies><hobbies>hiking</hobbies></root>").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("optional keys"):
                val schemaDefinition = """= {
                                            name: text
                                            age?: number
                                            hobbies: text*
                                        }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateYamlString(schema, "name: John\nage:    30  \nhobbies:\n  - reading\n  - hiking").isValid),
                            assertTrue(validateYamlString(schema, "name: John\n            \nhobbies:\n  - reading\n  - hiking").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("extra keys are allowed"):
                val schemaDefinition = """= {
                                            name: text
                                            age?: number
                                        }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateYamlString(schema, "name: John\nage:    30  \nhobbies:\n  - reading\n  - hiking").isValid),
                            assertTrue(validateYamlString(schema, "name: John\n           \n").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("comments in schema"):
                val schemaDefinition = """
                    |# This is a comment at the top
                    |= {
                    |    name: text    # inline comment after field
                    |    age: number
                    |    # comment on its own line
                    |    hobbies: text*
                    |}
                    |# trailing comment
                    |""".stripMargin
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """{"name": "John", "age": 30, "hobbies": ["reading"]}""").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("Text constraints")(
            test("length range"):
                val schemaDefinition = """= text [ 10 <= length <= 100 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateYamlString(schema, "Joe Clipperz").isValid),
                            assertTrue(validateYamlString(schema, "Joe").isValid == false),
                            assertTrue(validateYamlString(schema, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore.").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("regex"):
                val schemaDefinition = """= text [ regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})" ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateYamlString(schema, "2004-01-20").isValid),
                            assertTrue(validateYamlString(schema, "Joe").isValid == false),
                            assertTrue(validateYamlString(schema, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore.").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("regex with length constraint"):
                val schemaDefinition = """= text [ regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})", length == 10 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateYamlString(schema, "2004-01-20").isValid),
                            assertTrue(validateYamlString(schema, "Joe").isValid == false),
                            assertTrue(validateYamlString(schema, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore.").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("email regex"):
                val schemaDefinition = """= text [ regex = "^[\w\-\.]+@([\w-]+\.)+[\w-]{2,}$" ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateYamlString(schema, "joe@example.com").isValid),
                            assertTrue(validateYamlString(schema, "joe@example").isValid == false),
                            assertTrue(validateYamlString(schema, "joe.example.com").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("pattern"):
                val schemaDefinition = """= text [ pattern = "{###-###-####}" ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateString(schema, "{123-456-7890}").isValid),
                            assertTrue(validateString(schema, "joe@example").isValid == false),
                            assertTrue(validateString(schema, "joe.example.com").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("Number constraints")(
            test("integer"):
                val schemaDefinition = """= number [ integer ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "42").isValid),
                            assertTrue(validateJsonString(schema, "-10").isValid),
                            assertTrue(validateJsonString(schema, "0").isValid),
                            assertTrue(validateJsonString(schema, "3.14").isValid == false),
                            assertTrue(validateJsonString(schema, "0.5").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("min value (inclusive)"):
                val schemaDefinition = """= number [ value >= 0 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "0").isValid),
                            assertTrue(validateJsonString(schema, "100").isValid),
                            assertTrue(validateJsonString(schema, "0.5").isValid),
                            assertTrue(validateJsonString(schema, "-1").isValid == false),
                            assertTrue(validateJsonString(schema, "-0.001").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("min value (exclusive)"):
                val schemaDefinition = """= number [ value > 0 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "0").isValid == false),
                            assertTrue(validateJsonString(schema, "100").isValid),
                            assertTrue(validateJsonString(schema, "0.5").isValid),
                            assertTrue(validateJsonString(schema, "-1").isValid == false),
                            assertTrue(validateJsonString(schema, "-0.001").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("max value"):
                val schemaDefinition = """= number [ value <= 100 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "100").isValid),
                            assertTrue(validateJsonString(schema, "0").isValid),
                            assertTrue(validateJsonString(schema, "-50").isValid),
                            assertTrue(validateJsonString(schema, "101").isValid == false),
                            assertTrue(validateJsonString(schema, "100.001").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("range"):
                val schemaDefinition = """= number [ 0 <= value <= 100 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "0").isValid),
                            assertTrue(validateJsonString(schema, "50").isValid),
                            assertTrue(validateJsonString(schema, "100").isValid),
                            assertTrue(validateJsonString(schema, "-1").isValid == false),
                            assertTrue(validateJsonString(schema, "101").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("exact value"):
                val schemaDefinition = """= number [ value == 42 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "42").isValid),
                            assertTrue(validateJsonString(schema, "41").isValid == false),
                            assertTrue(validateJsonString(schema, "43").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("combined integer and range"):
                val schemaDefinition = """= number [ integer, 1 <= value <= 12 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "1").isValid),
                            assertTrue(validateJsonString(schema, "6").isValid),
                            assertTrue(validateJsonString(schema, "12").isValid),
                            assertTrue(validateJsonString(schema, "0").isValid == false),
                            assertTrue(validateJsonString(schema, "13").isValid == false),
                            assertTrue(validateJsonString(schema, "6.5").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("negative values"):
                val schemaDefinition = """= number [ value >= -10 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "-10").isValid),
                            assertTrue(validateJsonString(schema, "-5").isValid),
                            assertTrue(validateJsonString(schema, "0").isValid),
                            assertTrue(validateJsonString(schema, "-11").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("decimal bounds"):
                val schemaDefinition = """= number [ value >= 0.5 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, "0.5").isValid),
                            assertTrue(validateJsonString(schema, "1").isValid),
                            assertTrue(validateJsonString(schema, "0.49").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("List constraints")(
            test("zero or more (*)"):
                val schemaDefinition = """= text*"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", "bar"]""").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("one or more (+)"):
                val schemaDefinition = """= text+"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid == false),
                            assertTrue(validateJsonString(schema, """["foo", "bar"]""").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("exact size"):
                val schemaDefinition = """= text* [ size == 1 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid == false),
                            assertTrue(validateJsonString(schema, """["foo"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", "bar"]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("size range"):
                val schemaDefinition = """= text* [ 2 <= size <= 3 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid == false),
                            assertTrue(validateJsonString(schema, """["foo"]""").isValid == false),
                            assertTrue(validateJsonString(schema, """["foo", "bar"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", "bar", "tik"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", "bar", "tik", "tak"]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("size constraint only"):
                val schemaDefinition = """= text* [ 2 <= size <= 5 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """["foo", "bar"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", "bar", "baz"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo"]""").isValid == false),
                            assertTrue(validateJsonString(schema, """["a", "b", "c", "d", "e", "f"]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("List uniqueness constraints")(
            test("simple text values"):
                val schemaDefinition = """= text* [ unique ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", "bar", "baz"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", "bar", "foo"]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("simple number values"):
                val schemaDefinition = """= number+ [ unique ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[1, 2, 3]""").isValid),
                            assertTrue(validateJsonString(schema, """[1, 2, 1]""").isValid == false),
                            assertTrue(validateJsonString(schema, """[]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("object with single field key"):
                val schemaDefinition = """= { id: text, name: text }* [ unique = id ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "name": "Alice"}]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "name": "Alice"}, {"id": "2", "name": "Bob"}]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "name": "Alice"}, {"id": "2", "name": "Alice"}]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "name": "Alice"}, {"id": "1", "name": "Bob"}]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("object with composite key"):
                val schemaDefinition = """= { id: text, name: text }* [ unique = (id, name) ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "name": "Alice"}, {"id": "1", "name": "Bob"}]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "name": "Alice"}, {"id": "2", "name": "Alice"}]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "name": "Alice"}, {"id": "1", "name": "Alice"}]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("multiple independent constraints"):
                val schemaDefinition = """= { id: text, code: text, name: text }* [ unique = id, unique = code ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """[]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "code": "A", "name": "Alice"}]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "code": "A", "name": "Alice"}, {"id": "2", "code": "B", "name": "Bob"}]""").isValid),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "code": "A", "name": "Alice"}, {"id": "1", "code": "B", "name": "Bob"}]""").isValid == false),
                            assertTrue(validateJsonString(schema, """[{"id": "1", "code": "A", "name": "Alice"}, {"id": "2", "code": "A", "name": "Bob"}]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("combined with size constraint"):
                val schemaDefinition = """= text* [ unique, 2 <= size <= 5 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """["foo", "bar"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo", "bar", "baz"]""").isValid),
                            assertTrue(validateJsonString(schema, """["foo"]""").isValid == false),
                            assertTrue(validateJsonString(schema, """["foo", "bar", "foo"]""").isValid == false),
                            assertTrue(validateJsonString(schema, """["a", "b", "c", "d", "e", "f"]""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("Binary constraints")(
            test("without encoding accepts any text"):
                val schemaDefinition = """= binary"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """"SGVsbG8gV29ybGQ="""").isValid),
                            assertTrue(validateJsonString(schema, """"any random text"""").isValid),
                            assertTrue(validateJsonString(schema, """"not valid base64!!!"""").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("base64 encoding"):
                val schemaDefinition = """= binary [ encoding = 'base64' ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """"SGVsbG8gV29ybGQ="""").isValid),
                            assertTrue(validateJsonString(schema, """""""").isValid),
                            assertTrue(validateJsonString(schema, """"not valid base64!!!"""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("hex encoding"):
                val schemaDefinition = """= binary [ encoding = 'hex' ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """"48656c6c6f"""").isValid),
                            assertTrue(validateJsonString(schema, """""""").isValid),
                            assertTrue(validateJsonString(schema, """"48 65 6c 6c 6f"""").isValid),
                            assertTrue(validateJsonString(schema, """"48656c6c6"""").isValid == false),
                            assertTrue(validateJsonString(schema, """"ZZZZ"""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("base32 encoding"):
                val schemaDefinition = """= binary [ encoding = 'base32' ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """"JBSWY3DPEHPK3PXP"""").isValid),
                            assertTrue(validateJsonString(schema, """"12345678"""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("base64 with exact size"):
                val schemaDefinition = """= binary [ encoding = 'base64', bytes == 11 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """"SGVsbG8gV29ybGQ="""").isValid),
                            assertTrue(validateJsonString(schema, """"SGVsbG8="""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("base64 with size range"):
                val schemaDefinition = """= binary [ encoding = 'base64', 5 <= bytes <= 20 ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """"SGVsbG8gV29ybGQ="""").isValid),
                            assertTrue(validateJsonString(schema, """"SGk="""").isValid == false),
                            assertTrue(validateJsonString(schema, """"VGhpcyBpcyBhIHZlcnkgbG9uZyBzdHJpbmcgdGhhdCBpcyB0b28gYmlnIQ=="""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("in object field"):
                val schemaDefinition = """= { name: text, data: binary [ encoding = 'base64' ] }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """{"name": "test", "data": "SGVsbG8="}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "test", "data": "!!!invalid!!!"}""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("Default values")(
            test("missing field with default is valid"):
                val schemaDefinition = """= { name: text, status: text ?= "active" }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """{"name": "John", "status": "inactive"}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "John"}""").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("missing field without default is invalid"):
                val schemaDefinition = """= { name: text, status: text }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """{"name": "John", "status": "active"}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "John"}""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("numeric field with default"):
                val schemaDefinition = """= { name: text, count: number [ integer ] ?= 0 }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """{"name": "John", "count": 5}""").isValid),
                            assertTrue(validateJsonString(schema, """{"name": "John"}""").isValid)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("provided value must still match constraints"):
                val schemaDefinition = """= { status: text [ length <= 10 ] ?= "active" }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """{"status": "ok"}""").isValid),
                            assertTrue(validateJsonString(schema, """{}""").isValid),
                            assertTrue(validateJsonString(schema, """{"status": "this is way too long"}""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("Time constraints")(
            test("iso8601 datetime"):
                val schemaDefinition = """= time [ format = 'iso8601' ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateString(schema, "2023-12-25T10:30:00").isValid),
                            // assertTrue(validateString(schema, "2023-12-25T10:30:00Z").isValid),
                            // assertTrue(validateString(schema, "2023-12-25T10:30:00+01:00").isValid),
                            assertTrue(validateString(schema, "2023-12-25").isValid == false),
                            assertTrue(validateString(schema, "not a datetime").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("iso8601-date"):
                val schemaDefinition = """= time [ format = 'iso8601-date' ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateString(schema, "2023-12-25").isValid),
                            assertTrue(validateString(schema, "2023-01-01").isValid),
                            assertTrue(validateString(schema, "2023-13-01").isValid == false),
                            assertTrue(validateString(schema, "2023/12/25").isValid == false),
                            assertTrue(validateString(schema, "not a date").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("iso8601-time"):
                val schemaDefinition = """= time [ format = 'iso8601-time' ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateString(schema, "10:30:00").isValid),
                            assertTrue(validateString(schema, "23:59:59").isValid),
                            assertTrue(validateString(schema, "10:30").isValid),
                            assertTrue(validateString(schema, "25:00:00").isValid == false),
                            assertTrue(validateString(schema, "not a time").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("custom pattern yyyy-MM-dd"):
                val schemaDefinition = """= time [ format = "yyyy-MM-dd" ]"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateString(schema, "2023-12-25").isValid),
                            assertTrue(validateString(schema, "2023-01-01").isValid),
                            assertTrue(validateString(schema, "25/12/2023").isValid == false),
                            assertTrue(validateString(schema, "not a date").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("in object field"):
                val schemaDefinition = """= { created: time [ format = 'iso8601' ], birthday: time [ format = 'iso8601-date' ] }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        allSuccesses(
                            assertTrue(validateJsonString(schema, """{"created": "2023-12-25T10:30:00", "birthday": "1990-05-15"}""").isValid),
                            // assertTrue(validateJsonString(schema, """{"created": "2023-12-25T10:30:00Z", "birthday": "1990-05-15"}""").isValid),
                            assertTrue(validateJsonString(schema, """{"created": "not a datetime", "birthday": "1990-05-15"}""").isValid == false),
                            assertTrue(validateJsonString(schema, """{"created": "2023-12-25T10:30:00Z", "birthday": "not a date"}""").isValid == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
        ),
        suite("Deprecation warnings")(
            test("using deprecated field reports warning"):
                val schemaDefinition = """= { @deprecated oldField: text, newField: number }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        val result = validateJsonString(schema, """{"oldField": "test", "newField": 42}""")
                        allSuccesses(
                            assertTrue(result.isValid),
                            assertTrue(result.hasWarnings)
                        )
                    case Left(value)   => assertTrue(value == ""))
            ,
            test("not using deprecated field reports no warning"):
                val schemaDefinition = """= { @deprecated oldField?: text, newField: number }"""
                allSuccesses(parse(schemaDefinition) match
                    case Right(schema) =>
                        val result = validateJsonString(schema, """{"newField": 42}""")
                        allSuccesses(
                            assertTrue(result.isValid),
                            assertTrue(result.hasWarnings == false)
                        )
                    case Left(value)   => assertTrue(value == ""))
        )
    )

package rengbis

import zio.test.{ assertTrue, TestConsole, ZIOSpecDefault }
import zio.test.TestResult.allSuccesses
import zio.Chunk
import Schema.*
import Validator.*

object ValidatorSpec extends ZIOSpecDefault:
    val validateYamlString = Validator.validateString(DataParsers.yaml)
    val validateJsonString = Validator.validateString(DataParsers.json)
    val validateXmlString  = Validator.validateString(DataParsers.xml)
    val validateString     = Validator.validateString(DataParsers.text)

    def spec = suite("Validator features")(
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
            val schemaDefinition = """
= {
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
        test("simple values with optional keys"):
            val schemaDefinition = """
= {
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
        test("simple values with extra keys"):
            val schemaDefinition = """
= {
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
        test("text constraints (minLength, maxLength)"):
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
        test("text constraints (regex)"):
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
        test("text constraints (regex) with also length constraint"):
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
        test("text constraints (email regex)"):
            // val schemaDefinition ="""= text [ regex = "/^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/" ]"""
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
        test("text patterns"):
            val schemaDefinition = """= text [ pattern = "{###-###-####}" ]"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateString(schema, "{123-456-7890}").isValid),
                        assertTrue(validateString(schema, "joe@example").isValid == false),
                        assertTrue(validateString(schema, "joe.example.com").isValid == false)
                    )
                case Left(value)   => assertTrue(value == ""))
        ,
        test("list constraints `*`"):
            val schemaDefinition = """= text*"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateJsonString(schema, """[]""").isValid),
                        assertTrue(validateJsonString(schema, """["foo", "bar"]""").isValid)
                    )
                case Left(value)   => assertTrue(value == ""))
        ,
        test("list constraints `+`"):
            val schemaDefinition = """= text+"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateJsonString(schema, """[]""").isValid == false),
                        assertTrue(validateJsonString(schema, """["foo", "bar"]""").isValid)
                    )
                case Left(value)   => assertTrue(value == ""))
        ,
        test("list constraints `size == 1`"):
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
        test("list constraints `2 <= size <= 3`"):
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
        test("comments"):
            val schemaDefinition = """
# This is a comment at the top
= {
    name: text    # inline comment after field
    age: number
    # comment on its own line
    hobbies: text*
}
# trailing comment
"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateJsonString(schema, """{"name": "John", "age": 30, "hobbies": ["reading"]}""").isValid)
                    )
                case Left(value)   => assertTrue(value == ""))
        ,
        test("number constraints (integer)"):
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
        test("number constraints (min value)"):
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
        test("number constraints (min value) - exclusive"):
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
        test("number constraints (max value)"):
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
        test("number constraints (range)"):
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
        test("number constraints (exact value)"):
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
        test("number constraints (combined integer and range)"):
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
        test("number constraints (negative values)"):
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
        test("number constraints (decimal bounds)"):
            val schemaDefinition = """= number [ value >= 0.5 ]"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateJsonString(schema, "0.5").isValid),
                        assertTrue(validateJsonString(schema, "1").isValid),
                        assertTrue(validateJsonString(schema, "0.49").isValid == false)
                    )
                case Left(value)   => assertTrue(value == ""))
        ,
        test("list uniqueness constraint (simple text values)"):
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
        test("list uniqueness constraint (simple number values)"):
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
        test("list uniqueness constraint (object with single field key)"):
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
        test("list uniqueness constraint (object with composite key)"):
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
        test("list uniqueness constraint (multiple independent constraints)"):
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
        test("list uniqueness constraint combined with size constraint"):
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
        ,
        test("list size constraint only"):
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
    )

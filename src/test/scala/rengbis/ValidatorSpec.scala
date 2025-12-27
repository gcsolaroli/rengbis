package rengbis

import zio.test.{ TestConsole, ZIOSpecDefault, assertTrue }
import zio.test.TestResult.{ allSuccesses }
import zio.Chunk
import Schema.*
import Validator.*

object ValidatorSpec extends ZIOSpecDefault:
    val validateYamlString = Validator.validateString(DataParsers.yaml)
    val validateJsonString = Validator.validateString(DataParsers.json)
    val validateXmlString  = Validator.validateString(DataParsers.xml)
    val validateString     = Validator.validateString(DataParsers.text)

    def spec = suite("Validator features") (
        test("trivial Yaml values"):
            allSuccesses(parse("""= "yes" | "no" """) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateYamlString(schema, "yes").isValid),
                        assertTrue(validateYamlString(schema, "no") .isValid),
                        assertTrue(validateYamlString(schema, "🤷‍♂️") .isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("simple JSON/Yaml/XML values"):
            val schemaDefinition ="""
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
                        assertTrue(validateXmlString(schema, "<root><name>John</name><age>thirty</age><hobbies>reading</hobbies><hobbies>hiking</hobbies></root>").isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("simple values with optional keys"):
            val schemaDefinition ="""
= {
	name: text
	age?: number
	hobbies: text*
}"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateYamlString(schema, "name: John\nage:    30  \nhobbies:\n  - reading\n  - hiking").isValid),
                        assertTrue(validateYamlString(schema, "name: John\n            \nhobbies:\n  - reading\n  - hiking").isValid),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("simple values with extra keys"):
            val schemaDefinition ="""
= {
	name: text
	age?: number
}"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateYamlString(schema, "name: John\nage:    30  \nhobbies:\n  - reading\n  - hiking").isValid),
                        assertTrue(validateYamlString(schema,  "name: John\n           \n"                                 ).isValid),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("text constraints (minLength, maxLength)"):
            val schemaDefinition ="""= text { 10 <= length <= 100 }"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateYamlString(schema, "Joe Clipperz") .isValid),
                        assertTrue(validateYamlString(schema, "Joe").isValid == false),
                        assertTrue(validateYamlString(schema, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore.").isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("text constraints (regex)"):
            val schemaDefinition ="""= text { regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})" }"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateYamlString(schema, "2004-01-20") .isValid),
                        assertTrue(validateYamlString(schema, "Joe").isValid == false),
                        assertTrue(validateYamlString(schema, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore.").isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("text constraints (regex) with also length constraint"):
            val schemaDefinition ="""= text { regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})", length == 10 }"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateYamlString(schema, "2004-01-20") .isValid),
                        assertTrue(validateYamlString(schema, "Joe").isValid == false),
                        assertTrue(validateYamlString(schema, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore.").isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("text constraints (email regex)"):
            // val schemaDefinition ="""= text { regex = "/^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/" }"""
            val schemaDefinition ="""= text { regex = "^[\w\-\.]+@([\w-]+\.)+[\w-]{2,}$" }"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateYamlString(schema, "joe@example.com") .isValid),
                        assertTrue(validateYamlString(schema, "joe@example").isValid == false),
                        assertTrue(validateYamlString(schema, "joe.example.com").isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("text patterns"):
            val schemaDefinition ="""= text { pattern = "{###-###-####}"}"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateString(schema, "{123-456-7890}") .isValid),
                        assertTrue(validateString(schema, "joe@example").isValid == false),
                        assertTrue(validateString(schema, "joe.example.com").isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("list constraints `*`"):
            val schemaDefinition ="""= text*"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateJsonString(schema, """[]""") .isValid),
                        assertTrue(validateJsonString(schema, """["foo", "bar"]""") .isValid),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("list constraints `+`"):
            val schemaDefinition ="""= text+"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateJsonString(schema, """[]""").isValid == false),
                        assertTrue(validateJsonString(schema, """["foo", "bar"]""") .isValid),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("list constraints `{1}`"):
            val schemaDefinition ="""= text{1}"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateJsonString(schema, """[]""").isValid == false),
                        assertTrue(validateJsonString(schema, """["foo"]""") .isValid),
                        assertTrue(validateJsonString(schema, """["foo", "bar"]""") .isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
        test("list constraints `{2,3}`"):
            val schemaDefinition ="""= text{2,3}"""
            allSuccesses(parse(schemaDefinition) match
                case Right(schema) =>
                    allSuccesses(
                        assertTrue(validateJsonString(schema, """[]""").isValid == false),
                        assertTrue(validateJsonString(schema, """["foo"]""") .isValid == false),
                        assertTrue(validateJsonString(schema, """["foo", "bar"]""") .isValid),
                        assertTrue(validateJsonString(schema, """["foo", "bar", "tik"]""") .isValid),
                        assertTrue(validateJsonString(schema, """["foo", "bar", "tik", "tak"]""") .isValid == false),
                    )
                case Left(value) => assertTrue(value == "")
            )
        ,
    )

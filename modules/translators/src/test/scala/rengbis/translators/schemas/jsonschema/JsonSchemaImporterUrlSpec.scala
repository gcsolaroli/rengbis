package rengbis.translators.schemas.jsonschema

import rengbis.Schema.*
import rengbis.translators.common.SchemaFetcher
import zio.test.*

object JsonSchemaImporterUrlSpec extends ZIOSpecDefault:

    /** A mock fetcher that returns predefined responses for specific URLs. */
    class MockFetcher(responses: Map[String, Either[String, String]]) extends SchemaFetcher:
        var fetchCount: Map[String, Int] = Map.empty.withDefaultValue(0)

        def fetch(url: String): Either[String, String] =
            fetchCount = fetchCount + (url -> (fetchCount(url) + 1))
            responses.getOrElse(url, Left(s"URL not found: $url"))

    def spec = suite("JsonSchemaImporter URL fetching")(
        suite("Without fetcher (default behavior)")(
            test("URL reference reports loss when no fetcher configured") {
                val jsonSchema = """{
                    "type": "object",
                    "properties": {
                        "address": { "$ref": "https://example.com/address.json" }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema)
                assertTrue(
                    result.isRight,
                    result.exists(
                        _.report.entries.exists(e =>
                            e.message.contains("External URL reference") &&
                                e.message.contains("cannot be imported")
                        )
                    )
                )
            }
        ),
        suite("With mock fetcher")(
            test("fetches and translates external schema") {
                val fetcher = MockFetcher(
                    Map(
                        "https://example.com/address.json" -> Right("""{
                        "type": "object",
                        "properties": {
                            "street": { "type": "string" },
                            "city": { "type": "string" }
                        }
                    }""")
                    )
                )

                val jsonSchema = """{
                    "type": "object",
                    "properties": {
                        "address": { "$ref": "https://example.com/address.json" }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema, fetcher)

                assertTrue(
                    result.isRight,
                    result.exists { r =>
                        r.root match
                            case ObjectValue(props) =>
                                props.get(OptionalLabel("address")) match
                                    case Some(ObjectValue(addrProps)) =>
                                        addrProps.contains(OptionalLabel("street")) &&
                                        addrProps.contains(OptionalLabel("city"))
                                    case _                            => false
                            case _                  => false
                    }
                )
            },
            test("fetches and resolves fragment in external schema") {
                val fetcher = MockFetcher(
                    Map(
                        "https://example.com/defs.json" -> Right("""{
                        "$defs": {
                            "Name": { "type": "string", "minLength": 1 }
                        }
                    }""")
                    )
                )

                val jsonSchema = """{
                    "type": "object",
                    "properties": {
                        "name": { "$ref": "https://example.com/defs.json#/$defs/Name" }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema, fetcher)

                assertTrue(
                    result.isRight,
                    result.exists { r =>
                        r.root match
                            case ObjectValue(props) =>
                                props.get(OptionalLabel("name")) match
                                    case Some(TextValue(constraints, _)) =>
                                        // Check that minLength constraint was imported
                                        constraints.size.exists(_.min.exists(_.value == 1))
                                    case _                               => false
                            case _                  => false
                    }
                )
            },
            test("caches fetched schemas (same URL fetched only once)") {
                val fetcher = MockFetcher(
                    Map(
                        "https://example.com/common.json" -> Right("""{
                        "type": "string"
                    }""")
                    )
                )

                // Schema that references the same URL twice
                val jsonSchema = """{
                    "type": "object",
                    "properties": {
                        "field1": { "$ref": "https://example.com/common.json" },
                        "field2": { "$ref": "https://example.com/common.json" }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema, fetcher)

                assertTrue(
                    result.isRight,
                    fetcher.fetchCount("https://example.com/common.json") == 1
                )
            },
            test("fetchedUrls contains all URLs that were fetched") {
                val fetcher = MockFetcher(
                    Map(
                        "https://example.com/schema1.json" -> Right("""{ "type": "string" }"""),
                        "https://example.com/schema2.json" -> Right("""{ "type": "number" }""")
                    )
                )

                val jsonSchema = """{
                    "type": "object",
                    "properties": {
                        "field1": { "$ref": "https://example.com/schema1.json" },
                        "field2": { "$ref": "https://example.com/schema2.json" }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema, fetcher)

                assertTrue(
                    result.isRight,
                    result.exists(_.fetchedUrls == Set("https://example.com/schema1.json", "https://example.com/schema2.json"))
                )
            },
            test("reports loss on network error") {
                val fetcher = MockFetcher(
                    Map(
                        "https://example.com/broken.json" -> Left("Connection refused")
                    )
                )

                val jsonSchema = """{
                    "type": "object",
                    "properties": {
                        "data": { "$ref": "https://example.com/broken.json" }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema, fetcher)

                assertTrue(
                    result.isRight,
                    result.exists(
                        _.report.entries.exists(e =>
                            e.message.contains("Failed to fetch") &&
                                e.message.contains("Connection refused")
                        )
                    )
                )
            },
            test("reports loss on invalid JSON in fetched schema") {
                val fetcher = MockFetcher(
                    Map(
                        "https://example.com/invalid.json" -> Right("not valid json {{{")
                    )
                )

                val jsonSchema = """{
                    "type": "object",
                    "properties": {
                        "data": { "$ref": "https://example.com/invalid.json" }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema, fetcher)

                assertTrue(
                    result.isRight,
                    result.exists(_.report.entries.exists(e => e.message.contains("Failed to parse fetched schema")))
                )
            },
            test("reports loss on missing fragment in fetched schema") {
                val fetcher = MockFetcher(
                    Map(
                        "https://example.com/defs.json" -> Right("""{
                        "$defs": {
                            "Existing": { "type": "string" }
                        }
                    }""")
                    )
                )

                val jsonSchema = """{
                    "type": "object",
                    "properties": {
                        "data": { "$ref": "https://example.com/defs.json#/$defs/NonExistent" }
                    }
                }"""
                val result     = JsonSchemaImporter.fromJsonSchemaWithDefinitions(jsonSchema, fetcher)

                assertTrue(
                    result.isRight,
                    result.exists(
                        _.report.entries.exists(e =>
                            e.message.contains("Fragment") &&
                                e.message.contains("not found")
                        )
                    )
                )
            }
        )
    )

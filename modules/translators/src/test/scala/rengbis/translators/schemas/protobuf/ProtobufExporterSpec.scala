package rengbis.translators.schemas.protobuf

import zio.test.*
import rengbis.Schema.*
import rengbis.translators.common.FrictionType

object ProtobufExporterSpec extends ZIOSpecDefault:

    def spec = suite("Protobuf Exporter")(
        suite("Primitive types")(
            test("exports boolean as bool"):
                val schema = ObjectValue(Map(MandatoryLabel("active") -> BooleanValue()))
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.proto.contains("bool active = 1;")
                )
            ,
            test("exports text as string"):
                val schema = ObjectValue(Map(MandatoryLabel("name") -> TextValue()))
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.proto.contains("string name = 1;")
                )
            ,
            test("exports integer as int32"):
                val schema = ObjectValue(Map(MandatoryLabel("count") -> NumericValue(NumericConstraint.Constraints(integer = true))))
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.proto.contains("int32 count = 1;")
                )
            ,
            test("exports large integer as int64"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("bigNum") -> NumericValue(
                            NumericConstraint.Constraints(
                                integer = true,
                                value = Some(NumericConstraint.ValueRange.maxInclusive(BigDecimal(Long.MaxValue)))
                            )
                        )
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.proto.contains("int64 big_num = 1;")
                )
            ,
            test("exports number as double"):
                val schema = ObjectValue(Map(MandatoryLabel("price") -> NumericValue()))
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.proto.contains("double price = 1;")
                )
            ,
            test("exports binary as bytes"):
                val schema = ObjectValue(Map(MandatoryLabel("data") -> BinaryValue()))
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.proto.contains("bytes data = 1;")
                )
        ),
        suite("Complex types")(
            test("exports object as message"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        MandatoryLabel("age")  -> NumericValue(NumericConstraint.Constraints(integer = true))
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Person")
                assertTrue(
                    result.proto.contains("syntax = \"proto3\";") &&
                        result.proto.contains("message Person {") &&
                        result.proto.contains("string name =") &&
                        result.proto.contains("int32 age =")
                )
            ,
            test("exports optional field with optional keyword"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        OptionalLabel("email") -> TextValue()
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "User")
                assertTrue(
                    result.proto.contains("string name =") &&
                        result.proto.contains("optional string email =")
                )
            ,
            test("exports list as repeated"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("tags") -> ListOfValues(TextValue())
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Item")
                assertTrue(
                    result.proto.contains("repeated string tags = 1;")
                )
            ,
            test("exports map"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("metadata") -> MapValue(TextValue())
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Item")
                assertTrue(
                    result.proto.contains("map<string, string> metadata = 1;")
                )
            ,
            test("exports enum"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("status") -> EnumValues("ACTIVE", "INACTIVE", "PENDING")
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "User")
                assertTrue(
                    result.proto.contains("enum Status0 {") &&
                        result.proto.contains("ACTIVE = 0;") &&
                        result.proto.contains("INACTIVE = 1;") &&
                        result.proto.contains("PENDING = 2;")
                )
            ,
            test("exports string enum from alternatives"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("color") -> AlternativeValues(
                            GivenTextValue("red"),
                            GivenTextValue("green"),
                            GivenTextValue("blue")
                        )
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Palette")
                assertTrue(
                    result.proto.contains("enum Color0 {") &&
                        result.proto.contains("RED = 0;") &&
                        result.proto.contains("GREEN = 1;") &&
                        result.proto.contains("BLUE = 2;")
                )
        ),
        suite("Time types")(
            test("exports time as google.protobuf.Timestamp"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("createdAt") -> TimeValue(TimeConstraint.NamedFormat.ISO8601)
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Event")
                assertTrue(
                    result.proto.contains("import \"google/protobuf/timestamp.proto\";") &&
                        result.proto.contains("google.protobuf.Timestamp created_at = 1;")
                )
        ),
        suite("Any type")(
            test("exports any as google.protobuf.Any"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("payload") -> AnyValue()
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Message")
                assertTrue(
                    result.proto.contains("import \"google/protobuf/any.proto\";") &&
                        result.proto.contains("google.protobuf.Any payload = 1;")
                )
        ),
        suite("Alternatives and oneof")(
            test("exports complex alternatives as oneof"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("value") -> AlternativeValues(
                            TextValue(),
                            NumericValue(NumericConstraint.Constraints(integer = true))
                        )
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Variant")
                assertTrue(
                    result.proto.contains("oneof value {") &&
                        result.proto.contains("string option0 =") &&
                        result.proto.contains("int32 option1 =")
                )
        ),
        suite("Tuples")(
            test("exports tuple as nested message"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("pair") -> TupleValue(TextValue(), NumericValue(NumericConstraint.Constraints(integer = true)))
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Container")
                assertTrue(
                    result.proto.contains("message Pair0 {") &&
                        result.proto.contains("string field0 =") &&
                        result.proto.contains("int32 field1 =") &&
                        result.report.entries.exists(_.frictionType == FrictionType.Approximation)
                )
        ),
        suite("Nested objects")(
            test("exports nested object as nested message"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("address") -> ObjectValue(
                            Map(
                                MandatoryLabel("street") -> TextValue(),
                                MandatoryLabel("city")   -> TextValue()
                            )
                        )
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Person")
                assertTrue(
                    result.proto.contains("message Address0 {") &&
                        result.proto.contains("string street =") &&
                        result.proto.contains("string city =") &&
                        result.proto.contains("Address0 address =")
                )
        ),
        suite("Documentation")(
            test("preserves documentation as comments"):
                val schema = Documented(
                    Some("A person entity"),
                    ObjectValue(
                        Map(
                            MandatoryLabel("name") -> TextValue()
                        )
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Person")
                assertTrue(
                    result.proto.contains("// A person entity")
                )
            ,
            test("marks deprecated fields"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("oldField") -> Deprecated(TextValue()),
                        MandatoryLabel("newField") -> TextValue()
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.proto.contains("[deprecated = true]")
                )
        ),
        suite("Friction reporting")(
            test("reports friction for text constraints"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(TextConstraint.Constraints(size = Some(TextConstraint.SizeRange.maxInclusive(100))))
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.report.entries.exists(e =>
                        e.frictionType == FrictionType.Loss &&
                            e.message.contains("Text constraints")
                    )
                )
            ,
            test("reports friction for numeric constraints"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("age") -> NumericValue(
                            NumericConstraint.Constraints(
                                integer = true,
                                value = Some(NumericConstraint.ValueRange.minInclusive(BigDecimal(0)))
                            )
                        )
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Test")
                assertTrue(
                    result.report.entries.exists(e =>
                        e.frictionType == FrictionType.Loss &&
                            e.message.contains("Numeric range constraints")
                    )
                )
        ),
        suite("Package support")(
            test("includes package declaration"):
                val schema = ObjectValue(Map(MandatoryLabel("name") -> TextValue()))
                val result = ProtobufExporter.toProtobuf(schema, "Test", Some("com.example"))
                assertTrue(
                    result.proto.contains("package com.example;")
                )
        ),
        suite("Named references")(
            test("preserves named value references"):
                val schema = ObjectValue(
                    Map(
                        MandatoryLabel("user") -> NamedValueReference("User")
                    )
                )
                val result = ProtobufExporter.toProtobuf(schema, "Request")
                assertTrue(
                    result.proto.contains("User user = 1;")
                )
        ),
        suite("Multiple definitions")(
            test("exports multiple definitions"):
                val definitions = Map(
                    "user" -> ObjectValue(
                        Map(
                            MandatoryLabel("name")  -> TextValue(),
                            MandatoryLabel("email") -> TextValue()
                        )
                    ),
                    "post" -> ObjectValue(
                        Map(
                            MandatoryLabel("title")  -> TextValue(),
                            MandatoryLabel("author") -> NamedValueReference("user")
                        )
                    )
                )
                val result      = ProtobufExporter.toProtobufWithDefinitions(definitions, packageName = Some("blog"))
                assertTrue(
                    result.proto.contains("package blog;") &&
                        result.proto.contains("message User {") &&
                        result.proto.contains("message Post {") &&
                        result.proto.contains("User author =")
                )
        )
    )

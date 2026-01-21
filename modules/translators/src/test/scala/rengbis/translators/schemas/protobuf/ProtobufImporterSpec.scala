package rengbis.translators.schemas.protobuf

import zio.test.*
import rengbis.Schema.*
import rengbis.translators.common.FrictionType

object ProtobufImporterSpec extends ZIOSpecDefault:

    def spec = suite("Protobuf Importer")(
        suite("Primitive types")(
            test("imports bool as boolean"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        bool active = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Test").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("active")).contains(BooleanValue())
                        case _                   => false
                    }
                })
            ,
            test("imports string as text"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        string name = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Test").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("name")).contains(TextValue())
                        case _                   => false
                    }
                })
            ,
            test("imports int32 as integer"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        int32 count = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Test").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("count")).contains(NumericValue(NumericConstraint.Constraints(integer = true)))
                        case _                   => false
                    }
                })
            ,
            test("imports int64 as integer"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        int64 bigNum = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Test").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("bigNum")).contains(NumericValue(NumericConstraint.Constraints(integer = true)))
                        case _                   => false
                    }
                })
            ,
            test("imports double as number"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        double price = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Test").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("price")).contains(NumericValue())
                        case _                   => false
                    }
                })
            ,
            test("imports bytes as binary"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        bytes data = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Test").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("data")).contains(BinaryValue())
                        case _                   => false
                    }
                })
        ),
        suite("Complex types")(
            test("imports message as object"):
                val proto  = """
                    syntax = "proto3";
                    message Person {
                        string name = 1;
                        int32 age = 2;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.contains("Person") &&
                    r.definitions.get("Person").exists {
                        case ObjectValue(fields) => fields.size == 2
                        case _                   => false
                    }
                })
            ,
            test("imports optional field"):
                val proto  = """
                    syntax = "proto3";
                    message User {
                        string name = 1;
                        optional string email = 2;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("User").exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("name")) &&
                            fields.contains(OptionalLabel("email"))
                        case _                   => false
                    }
                })
            ,
            test("imports repeated as list"):
                val proto  = """
                    syntax = "proto3";
                    message Item {
                        repeated string tags = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Item").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("tags")).exists {
                                case ListOfValues(TextValue(_, _), _) => true
                                case _                                => false
                            }
                        case _                   => false
                    }
                })
            ,
            test("imports map"):
                val proto  = """
                    syntax = "proto3";
                    message Item {
                        map<string, string> metadata = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Item").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("metadata")).exists {
                                case MapValue(TextValue(_, _)) => true
                                case _                         => false
                            }
                        case _                   => false
                    }
                })
            ,
            test("imports enum"):
                val proto  = """
                    syntax = "proto3";
                    enum Status {
                        UNKNOWN = 0;
                        ACTIVE = 1;
                        INACTIVE = 2;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Status").exists {
                        case EnumValues(values*) => values.size == 3
                        case _                   => false
                    }
                })
        ),
        suite("Well-known types")(
            test("imports google.protobuf.Timestamp as time"):
                val proto  = """
                    syntax = "proto3";
                    import "google/protobuf/timestamp.proto";
                    message Event {
                        google.protobuf.Timestamp created_at = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Event").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("createdAt")).exists {
                                case TimeValue(_*) => true
                                case _             => false
                            }
                        case _                   => false
                    }
                })
            ,
            test("imports google.protobuf.Any as any"):
                val proto  = """
                    syntax = "proto3";
                    import "google/protobuf/any.proto";
                    message Message {
                        google.protobuf.Any payload = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Message").exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("payload")).contains(AnyValue())
                        case _                   => false
                    }
                })
        ),
        suite("Oneof")(
            test("imports oneof as optional alternative"):
                val proto  = """
                    syntax = "proto3";
                    message Result {
                        oneof value {
                            string text = 1;
                            int32 number = 2;
                        }
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Result").exists {
                        case ObjectValue(fields) =>
                            fields.get(OptionalLabel("value")).exists {
                                case AlternativeValues(opts*) => opts.size == 2
                                case _                        => false
                            }
                        case _                   => false
                    }
                })
        ),
        suite("Nested definitions")(
            test("imports nested enum"):
                val proto  = """
                    syntax = "proto3";
                    message User {
                        enum Status {
                            UNKNOWN = 0;
                            ACTIVE = 1;
                        }
                        string name = 1;
                        Status status = 2;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.contains("User.Status") &&
                    r.definitions.contains("User")
                })
            ,
            test("imports nested message"):
                val proto  = """
                    syntax = "proto3";
                    message Person {
                        message Address {
                            string street = 1;
                            string city = 2;
                        }
                        string name = 1;
                        Address address = 2;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.contains("Person.Address") &&
                    r.definitions.contains("Person")
                })
        ),
        suite("Multiple messages")(
            test("imports multiple top-level messages"):
                val proto  = """
                    syntax = "proto3";
                    message User {
                        string name = 1;
                    }
                    message Post {
                        string title = 1;
                        User author = 2;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.contains("User") &&
                    r.definitions.contains("Post")
                })
        ),
        suite("Comments")(
            test("handles single-line comments"):
                val proto  = """
                    syntax = "proto3";
                    // This is a comment
                    message Test {
                        string name = 1; // field comment
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists(_.definitions.contains("Test")))
            ,
            test("handles multi-line comments"):
                val proto  = """
                    syntax = "proto3";
                    /* This is a
                       multi-line comment */
                    message Test {
                        string name = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists(_.definitions.contains("Test")))
        ),
        suite("Friction reporting")(
            test("reports field number loss"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        string name = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.report.entries.exists(e =>
                        e.frictionType == FrictionType.Loss &&
                            e.message.contains("Field numbers")
                    )
                })
            ,
            test("reports unsigned integer approximation"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        uint32 count = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.report.entries.exists(e =>
                        e.frictionType == FrictionType.Approximation &&
                            e.message.contains("Unsigned")
                    )
                })
        ),
        suite("Error handling")(
            test("reports error for missing syntax"):
                val proto  = """
                    message Test {
                        string name = 1;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isLeft)
        ),
        suite("Field name conversion")(
            test("converts snake_case to camelCase"):
                val proto  = """
                    syntax = "proto3";
                    message Test {
                        string user_name = 1;
                        int32 created_at_timestamp = 2;
                    }
                """
                val result = ProtobufImporter.fromProtobuf(proto)
                assertTrue(result.isRight && result.exists { r =>
                    r.definitions.get("Test").exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("userName")) &&
                            fields.contains(MandatoryLabel("createdAtTimestamp"))
                        case _                   => false
                    }
                })
        )
    )

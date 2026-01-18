package rengbis.translators.schemas.xsd

import rengbis.Schema.*
import rengbis.translators.common.*
import zio.test.*
import Assertion.*
import scala.xml.XML

object XsdExporterSpec extends ZIOSpecDefault:

    def spec = suite("XsdExporter")(
        suite("Basic types")(
            test("exports string type") {
                val schema        = TextValue()
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:string"),
                    xsd.contains("<xs:restriction base=\"xs:string\""),
                    report.isEmpty
                )
            },
            test("exports number type") {
                val schema        = NumericValue()
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:decimal"),
                    xsd.contains("<xs:restriction base=\"xs:decimal\""),
                    report.isEmpty
                )
            },
            test("exports integer type") {
                val schema        = NumericValue(NumericConstraint.Constraints(integer = true))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:integer"),
                    xsd.contains("<xs:restriction base=\"xs:integer\""),
                    report.isEmpty
                )
            },
            test("exports boolean type") {
                val schema        = BooleanValue()
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:boolean"),
                    xsd.contains("<xs:restriction base=\"xs:boolean\""),
                    report.isEmpty
                )
            },
            test("exports AnyValue") {
                val schema        = AnyValue()
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:any"),
                    xsd.contains("mixed=\"true\""),
                    report.isEmpty
                )
            },
            test("reports friction for Fail") {
                val schema        = Fail()
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:pattern"),
                    report.nonEmpty,
                    report.entries.exists(_.message.contains("fail"))
                )
            }
        ),
        suite("Text constraints")(
            test("exports minLength") {
                val schema        = TextValue(TextConstraint.Constraints(size = Some(TextConstraint.SizeRange.minInclusive(3))))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:minLength value=\"3\""),
                    report.isEmpty
                )
            },
            test("exports maxLength") {
                val schema        = TextValue(TextConstraint.Constraints(size = Some(TextConstraint.SizeRange.maxInclusive(100))))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:maxLength value=\"100\""),
                    report.isEmpty
                )
            },
            test("exports exact length") {
                val schema        = TextValue(TextConstraint.Constraints(size = Some(TextConstraint.SizeRange.exact(10))))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:length value=\"10\""),
                    report.isEmpty
                )
            },
            test("exports regex pattern") {
                val schema        = TextValue(TextConstraint.Constraints(regex = Some("[a-z]+")))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:pattern value=\"[a-z]+\""),
                    report.isEmpty
                )
            },
            test("exports email format as pattern") {
                val schema        = TextValue(TextConstraint.Constraints(format = Some("email")))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:pattern value=\"[^@]+@[^@]+\\.[^@]+\""),
                    report.isEmpty
                )
            },
            test("exports default value in annotation") {
                val schema        = TextValue(default = Some("default text"))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("Default: default text"),
                    report.isEmpty
                )
            },
            test("reports friction for custom format") {
                val schema        = TextValue(TextConstraint.Constraints(format = Some("custom")))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    report.nonEmpty,
                    report.entries.exists(_.message.contains("custom"))
                )
            }
        ),
        suite("Time/Date formats")(
            test("exports date format") {
                val schema        = TextValue(TextConstraint.Constraints(format = Some("iso8601-date")))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:date"),
                    xsd.contains("<xs:restriction base=\"xs:date\""),
                    report.isEmpty
                )
            },
            test("exports time format") {
                val schema        = TextValue(TextConstraint.Constraints(format = Some("iso8601-time")))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:time"),
                    xsd.contains("<xs:restriction base=\"xs:time\""),
                    report.isEmpty
                )
            },
            test("exports date-time format") {
                val schema        = TextValue(TextConstraint.Constraints(format = Some("iso8601-datetime")))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:dateTime"),
                    xsd.contains("<xs:restriction base=\"xs:dateTime\""),
                    report.isEmpty
                )
            },
            test("exports URI format") {
                val schema        = TextValue(TextConstraint.Constraints(format = Some("uri")))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:anyURI"),
                    xsd.contains("<xs:restriction base=\"xs:anyURI\""),
                    report.isEmpty
                )
            }
        ),
        suite("Numeric constraints")(
            test("exports minimum value") {
                val schema        = NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.minInclusive(BigDecimal(0)))))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:minInclusive value=\"0\""),
                    report.isEmpty
                )
            },
            test("exports maximum value") {
                val schema        = NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.maxInclusive(BigDecimal(100)))))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:maxInclusive value=\"100\""),
                    report.isEmpty
                )
            },
            test("exports exact value") {
                val schema        = NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.exact(BigDecimal(42)))))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:minInclusive value=\"42\""),
                    xsd.contains("<xs:maxInclusive value=\"42\""),
                    report.isEmpty
                )
            },
            test("exports integer with range") {
                val schema        = NumericValue(
                    NumericConstraint.Constraints(
                        value = Some(
                            NumericConstraint.ValueRange(
                                Some(BoundConstraint(BoundOp.MinInclusive, BigDecimal(1))),
                                Some(BoundConstraint(BoundOp.MaxInclusive, BigDecimal(10)))
                            )
                        ),
                        integer = true
                    )
                )
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xs:integer"),
                    xsd.contains("<xs:minInclusive value=\"1\""),
                    xsd.contains("<xs:maxInclusive value=\"10\""),
                    report.isEmpty
                )
            },
            test("exports default value in annotation") {
                val schema        = NumericValue(default = Some(BigDecimal(42)))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("Default: 42"),
                    report.isEmpty
                )
            },
            test("reports friction for exclusive bounds") {
                val schema        = NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.minExclusive(BigDecimal(0)))))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    report.nonEmpty,
                    report.entries.exists(_.message.contains("Exclusive"))
                )
            }
        ),
        suite("Enum")(
            test("exports enum values") {
                val schema        = EnumValues("red", "green", "blue")
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:enumeration value=\"red\""),
                    xsd.contains("<xs:enumeration value=\"green\""),
                    xsd.contains("<xs:enumeration value=\"blue\""),
                    report.isEmpty
                )
            },
            test("exports const string") {
                val schema        = GivenTextValue("constant")
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:enumeration"),
                    report.isEmpty
                )
            }
        ),
        suite("Arrays")(
            test("exports simple array") {
                val schema        = ListOfValues(TextValue())
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:element name=\"item\""),
                    xsd.contains("minOccurs=\"0\""),
                    xsd.contains("maxOccurs=\"unbounded\""),
                    report.isEmpty
                )
            },
            test("exports array with minItems") {
                val schema        = ListOfValues(
                    TextValue(),
                    ListConstraint.Constraints(size = Some(ListConstraint.SizeRange.minInclusive(1)))
                )
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("minOccurs=\"1\""),
                    xsd.contains("maxOccurs=\"unbounded\""),
                    report.isEmpty
                )
            },
            test("exports array with maxItems") {
                val schema        = ListOfValues(
                    TextValue(),
                    ListConstraint.Constraints(size = Some(ListConstraint.SizeRange.maxInclusive(10)))
                )
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("minOccurs=\"0\""),
                    xsd.contains("maxOccurs=\"10\""),
                    report.isEmpty
                )
            },
            test("reports friction for unique") {
                val schema        = ListOfValues(
                    TextValue(),
                    ListConstraint.Constraints(unique = Seq(ListConstraint.Uniqueness.Simple))
                )
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    report.nonEmpty,
                    report.entries.exists(_.message.contains("Unique"))
                )
            }
        ),
        suite("Tuples")(
            test("exports tuple") {
                val schema        = TupleValue(TextValue(), NumericValue())
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:element name=\"item0\""),
                    xsd.contains("<xs:element name=\"item1\""),
                    xsd.contains("xs:string"),
                    xsd.contains("xs:decimal"),
                    report.isEmpty
                )
            }
        ),
        suite("Objects")(
            test("exports simple object") {
                val schema        = ObjectValue(
                    Map(
                        MandatoryLabel("name") -> TextValue(),
                        MandatoryLabel("age")  -> NumericValue(NumericConstraint.Constraints(integer = true))
                    )
                )
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:element name=\"name\""),
                    xsd.contains("<xs:element name=\"age\""),
                    xsd.contains("minOccurs=\"1\""),
                    report.isEmpty
                )
            },
            test("exports object with optional field") {
                val schema        = ObjectValue(
                    Map(
                        MandatoryLabel("required") -> TextValue(),
                        OptionalLabel("optional")  -> TextValue()
                    )
                )
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:element name=\"required\"") && xsd.contains("minOccurs=\"1\""),
                    xsd.contains("<xs:element name=\"optional\"") && xsd.contains("minOccurs=\"0\""),
                    report.isEmpty
                )
            }
        ),
        suite("Map")(
            test("exports map as key-value pairs") {
                val schema        = MapValue(NumericValue())
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:element name=\"entry\""),
                    xsd.contains("<xs:element name=\"key\""),
                    xsd.contains("<xs:element name=\"value\""),
                    report.nonEmpty,
                    report.entries.exists(_.message.contains("Map type"))
                )
            }
        ),
        suite("Alternative values")(
            test("exports alternatives as choice") {
                val schema        = AlternativeValues(
                    TextValue(),
                    NumericValue()
                )
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:choice>"),
                    xsd.contains("<xs:element name=\"choice0\""),
                    xsd.contains("<xs:element name=\"choice1\""),
                    report.isEmpty
                )
            }
        ),
        suite("References")(
            test("exports named reference") {
                val schema        = NamedValueReference("EmailType")
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("base=\"EmailType\""),
                    report.isEmpty
                )
            }
        ),
        suite("Metadata")(
            test("exports description") {
                val schema        = Documented(Some("A person's name"), TextValue())
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:documentation>A person's name</xs:documentation>"),
                    report.isEmpty
                )
            },
            test("exports deprecated") {
                val schema        = Deprecated(TextValue())
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:documentation>DEPRECATED</xs:documentation>"),
                    report.isEmpty
                )
            },
            test("exports both description and deprecated") {
                val schema        = Documented(Some("Old field"), Deprecated(TextValue()))
                val (xsd, report) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("<xs:documentation>Old field</xs:documentation>"),
                    xsd.contains("<xs:documentation>DEPRECATED</xs:documentation>"),
                    report.isEmpty
                )
            }
        ),
        suite("XSD structure")(
            test("generates valid XML") {
                val schema   = TextValue()
                val (xsd, _) = XsdExporter.toXsd(schema)
                val parsed   = XML.loadString(xsd)
                assertTrue(
                    parsed.label == "schema",
                    (parsed \ "@elementFormDefault").text == "qualified"
                )
            },
            test("includes root element") {
                val schema   = TextValue()
                val (xsd, _) = XsdExporter.toXsd(schema, "customRoot")
                assertTrue(
                    xsd.contains("<xs:element name=\"customRoot\""),
                    xsd.contains("type=\"RootType\"")
                )
            },
            test("declares XML Schema namespace") {
                val schema   = TextValue()
                val (xsd, _) = XsdExporter.toXsd(schema)
                assertTrue(
                    xsd.contains("xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"")
                )
            }
        )
    )

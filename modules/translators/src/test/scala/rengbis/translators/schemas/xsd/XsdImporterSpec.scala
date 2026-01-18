package rengbis.translators.schemas.xsd

import rengbis.Schema.*
import rengbis.translators.common.*
import zio.test.*
import Assertion.*

object XsdImporterSpec extends ZIOSpecDefault:

    def spec = suite("XsdImporter")(
        suite("Basic types")(
            test("imports string type") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root" type="xs:string"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists(_.isInstanceOf[TextValue])
                )
            },
            test("imports integer type") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root" type="xs:integer"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists(s => s.isInstanceOf[NumericValue])
                )
            },
            test("imports boolean type") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root" type="xs:boolean"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(BooleanValue())
                )
            },
            test("imports dateTime type") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root" type="xs:dateTime"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case TextValue(constraints, _) =>
                            constraints.format.contains("iso8601-datetime")
                        case _                         => false
                    }
                )
            }
        ),
        suite("Constraints")(
            test("imports minLength constraint") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:simpleType>
                    |      <xs:restriction base="xs:string">
                    |        <xs:minLength value="3"/>
                    |      </xs:restriction>
                    |    </xs:simpleType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case TextValue(constraints, _) =>
                            constraints.size.exists(s => s.min.exists(b => b.isMinInclusive && b.value == 3))
                        case _                         => false
                    }
                )
            },
            test("imports pattern constraint") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:simpleType>
                    |      <xs:restriction base="xs:string">
                    |        <xs:pattern value="[a-z]+"/>
                    |      </xs:restriction>
                    |    </xs:simpleType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case TextValue(constraints, _) =>
                            constraints.regex.contains("[a-z]+")
                        case _                         => false
                    }
                )
            },
            test("imports numeric range constraints") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:simpleType>
                    |      <xs:restriction base="xs:integer">
                    |        <xs:minInclusive value="1"/>
                    |        <xs:maxInclusive value="100"/>
                    |      </xs:restriction>
                    |    </xs:simpleType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case NumericValue(constraints, _) =>
                            val hasMin = constraints.value.exists(v => v.min.exists(b => b.isMinInclusive && b.value == BigDecimal(1)))
                            val hasMax = constraints.value.exists(v => v.max.exists(b => b.isMaxInclusive && b.value == BigDecimal(100)))
                            hasMin && hasMax
                        case _                            => false
                    }
                )
            }
        ),
        suite("Enumerations")(
            test("imports enumeration") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:simpleType>
                    |      <xs:restriction base="xs:string">
                    |        <xs:enumeration value="red"/>
                    |        <xs:enumeration value="green"/>
                    |        <xs:enumeration value="blue"/>
                    |      </xs:restriction>
                    |    </xs:simpleType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1) == Right(EnumValues("red", "green", "blue"))
                )
            }
        ),
        suite("Sequences and Objects")(
            test("imports sequence as object") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="name" type="xs:string"/>
                    |        <xs:element name="age" type="xs:integer"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("name")) &&
                            fields.contains(MandatoryLabel("age"))
                        case _                   => false
                    }
                )
            },
            test("imports optional elements") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="required" type="xs:string"/>
                    |        <xs:element name="optional" type="xs:string" minOccurs="0"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("required")) &&
                            fields.contains(OptionalLabel("optional"))
                        case _                   => false
                    }
                )
            }
        ),
        suite("Choice (Alternatives)")(
            test("imports choice as alternatives") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:choice>
                    |        <xs:element name="email" type="xs:string"/>
                    |        <xs:element name="phone" type="xs:string"/>
                    |      </xs:choice>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case AlternativeValues(schemas @ _*) => schemas.size == 2
                        case _                               => false
                    }
                )
            }
        ),
        suite("Arrays (Repeated Elements)")(
            test("imports unbounded element as list") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="item" type="xs:string" maxOccurs="unbounded"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("item")).exists {
                                case ListOfValues(_, _) => true
                                case _                  => false
                            }
                        case _                   => false
                    }
                )
            },
            test("imports list with minOccurs constraint") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="item" type="xs:string" minOccurs="1" maxOccurs="unbounded"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("item")).exists {
                                case ListOfValues(_, constraints) =>
                                    constraints.size.exists(s => s.min.exists(b => b.isMinInclusive && b.value == 1))
                                case _                            => false
                            }
                        case _                   => false
                    }
                )
            }
        ),
        suite("Named Types")(
            test("imports named simple type") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:simpleType name="EmailType">
                    |    <xs:restriction base="xs:string">
                    |      <xs:pattern value="[^@]+@[^@]+"/>
                    |    </xs:restriction>
                    |  </xs:simpleType>
                    |  <xs:element name="root" type="EmailType"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case TextValue(constraints, _) =>
                            constraints.regex.contains("[^@]+@[^@]+")
                        case _                         => false
                    }
                )
            },
            test("imports named complex type") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:complexType name="PersonType">
                    |    <xs:sequence>
                    |      <xs:element name="name" type="xs:string"/>
                    |      <xs:element name="age" type="xs:integer"/>
                    |    </xs:sequence>
                    |  </xs:complexType>
                    |  <xs:element name="root" type="PersonType"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("name")) &&
                            fields.contains(MandatoryLabel("age"))
                        case _                   => false
                    }
                )
            }
        ),
        suite("Friction reporting")(
            test("reports friction for ID type") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root" type="xs:ID"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._2.nonEmpty) == Right(true),
                    result.map(_._2.entries.exists(_.message.contains("ID"))).getOrElse(false)
                )
            },
            test("reports friction for xs:all") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:all>
                    |        <xs:element name="field1" type="xs:string"/>
                    |        <xs:element name="field2" type="xs:string"/>
                    |      </xs:all>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._2.nonEmpty) == Right(true),
                    result.map(_._2.entries.exists(_.message.contains("unordered"))).getOrElse(false)
                )
            },
            test("reports friction for exclusive bounds") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:simpleType>
                    |      <xs:restriction base="xs:integer">
                    |        <xs:minExclusive value="0"/>
                    |      </xs:restriction>
                    |    </xs:simpleType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._2.nonEmpty) == Right(true),
                    result.map(_._2.entries.exists(_.message.contains("Exclusive"))).getOrElse(false)
                )
            }
        ),
        suite("Error handling")(
            test("handles invalid XML") {
                val xsd    = """not valid xml"""
                val result = XsdImporter.fromXsd(xsd)
                assertTrue(result.isLeft)
            },
            test("handles non-schema root") {
                val xsd    = """<?xml version="1.0"?>
                    |<root xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <element name="test"/>
                    |</root>""".stripMargin
                val result = XsdImporter.fromXsd(xsd)
                assertTrue(result.isLeft)
            }
        )
    )

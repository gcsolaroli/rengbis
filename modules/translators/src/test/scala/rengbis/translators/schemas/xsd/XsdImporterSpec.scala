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
        ),
        suite("Attributes")(
            test("imports required attribute") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:attribute name="id" type="xs:string" use="required"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("@id")) &&
                            fields.get(MandatoryLabel("@id")).exists(_.isInstanceOf[TextValue])
                        case _                   => false
                    }
                )
            },
            test("imports optional attribute") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:attribute name="lang" type="xs:string"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(OptionalLabel("@lang"))
                        case _                   => false
                    }
                )
            },
            test("imports attribute with typed value") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:attribute name="count" type="xs:integer" use="required"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.get(MandatoryLabel("@count")).exists(_.isInstanceOf[NumericValue])
                        case _                   => false
                    }
                )
            },
            test("imports attribute with fixed value as enum") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:attribute name="version" type="xs:string" fixed="1.0"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.get(OptionalLabel("@version")).exists {
                                case EnumValues(values @ _*) => values == Seq("1.0")
                                case _                       => false
                            }
                        case _                   => false
                    }
                )
            },
            test("imports attribute with default value") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:attribute name="encoding" type="xs:string" default="UTF-8"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.get(OptionalLabel("@encoding")).exists {
                                case TextValue(_, Some(default)) => default == "UTF-8"
                                case _                           => false
                            }
                        case _                   => false
                    }
                )
            },
            test("imports attributes with sequence elements") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="person">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="name" type="xs:string"/>
                    |      </xs:sequence>
                    |      <xs:attribute name="id" type="xs:integer" use="required"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("name")) &&
                            fields.contains(MandatoryLabel("@id"))
                        case _                   => false
                    }
                )
            },
            test("imports simpleContent with extension and attributes") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="price">
                    |    <xs:complexType>
                    |      <xs:simpleContent>
                    |        <xs:extension base="xs:decimal">
                    |          <xs:attribute name="currency" type="xs:string" use="required"/>
                    |        </xs:extension>
                    |      </xs:simpleContent>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("_value")) &&
                            fields.contains(MandatoryLabel("@currency")) &&
                            fields.get(MandatoryLabel("_value")).exists(_.isInstanceOf[NumericValue])
                        case _                   => false
                    }
                )
            },
            test("imports attributeGroup reference") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:attributeGroup name="commonAttrs">
                    |    <xs:attribute name="id" type="xs:string"/>
                    |    <xs:attribute name="class" type="xs:string"/>
                    |  </xs:attributeGroup>
                    |  <xs:element name="div">
                    |    <xs:complexType>
                    |      <xs:attributeGroup ref="commonAttrs"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(OptionalLabel("@id")) &&
                            fields.contains(OptionalLabel("@class"))
                        case _                   => false
                    }
                )
            },
            test("imports attributeGroup with nested attributeGroup") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:attributeGroup name="baseAttrs">
                    |    <xs:attribute name="id" type="xs:string" use="required"/>
                    |  </xs:attributeGroup>
                    |  <xs:attributeGroup name="extendedAttrs">
                    |    <xs:attributeGroup ref="baseAttrs"/>
                    |    <xs:attribute name="name" type="xs:string"/>
                    |  </xs:attributeGroup>
                    |  <xs:element name="item">
                    |    <xs:complexType>
                    |      <xs:attributeGroup ref="extendedAttrs"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("@id")) &&
                            fields.contains(OptionalLabel("@name"))
                        case _                   => false
                    }
                )
            }
        ),
        suite("Groups")(
            test("imports group reference in sequence") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:group name="personName">
                    |    <xs:sequence>
                    |      <xs:element name="firstName" type="xs:string"/>
                    |      <xs:element name="lastName" type="xs:string"/>
                    |    </xs:sequence>
                    |  </xs:group>
                    |  <xs:element name="person">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:group ref="personName"/>
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
                            fields.contains(MandatoryLabel("firstName")) &&
                            fields.contains(MandatoryLabel("lastName")) &&
                            fields.contains(MandatoryLabel("age"))
                        case _                   => false
                    }
                )
            },
            test("imports group reference in choice") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:group name="addressGroup">
                    |    <xs:sequence>
                    |      <xs:element name="street" type="xs:string"/>
                    |      <xs:element name="city" type="xs:string"/>
                    |    </xs:sequence>
                    |  </xs:group>
                    |  <xs:element name="contact">
                    |    <xs:complexType>
                    |      <xs:choice>
                    |        <xs:element name="email" type="xs:string"/>
                    |        <xs:group ref="addressGroup"/>
                    |      </xs:choice>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case AlternativeValues(schemas @ _*) =>
                            schemas.size == 2
                        case _                               => false
                    }
                )
            },
            test("imports group with choice content") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:group name="contactChoice">
                    |    <xs:choice>
                    |      <xs:element name="email" type="xs:string"/>
                    |      <xs:element name="phone" type="xs:string"/>
                    |    </xs:choice>
                    |  </xs:group>
                    |  <xs:element name="person">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="name" type="xs:string"/>
                    |        <xs:group ref="contactChoice"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case TupleValue(schemas @ _*) =>
                            schemas.size == 2 &&
                            schemas.head.isInstanceOf[TextValue] &&
                            schemas(1).isInstanceOf[AlternativeValues]
                        case _                        => false
                    }
                )
            }
        ),
        suite("Element References")(
            test("imports element ref in sequence") {
                // Note: Root element is the first xs:element in document order
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="person">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element ref="firstName"/>
                    |        <xs:element ref="lastName"/>
                    |        <xs:element name="age" type="xs:integer"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |  <xs:element name="firstName" type="xs:string"/>
                    |  <xs:element name="lastName" type="xs:string"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("firstName")) &&
                            fields.contains(MandatoryLabel("lastName")) &&
                            fields.contains(MandatoryLabel("age")) &&
                            fields.get(MandatoryLabel("firstName")).exists(_.isInstanceOf[TextValue])
                        case _                   => false
                    }
                )
            },
            test("imports element ref with complex type") {
                // Note: Root element is the first xs:element in document order
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="person">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="name" type="xs:string"/>
                    |        <xs:element ref="address"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |  <xs:element name="address">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="street" type="xs:string"/>
                    |        <xs:element name="city" type="xs:string"/>
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
                            fields.contains(MandatoryLabel("address")) &&
                            fields.get(MandatoryLabel("address")).exists {
                                case ObjectValue(addrFields) =>
                                    addrFields.contains(MandatoryLabel("street")) &&
                                    addrFields.contains(MandatoryLabel("city"))
                                case _                       => false
                            }
                        case _                   => false
                    }
                )
            },
            test("imports optional element ref") {
                // Note: Root element is the first xs:element in document order
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="person">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="name" type="xs:string"/>
                    |        <xs:element ref="nickname" minOccurs="0"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |  <xs:element name="nickname" type="xs:string"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("name")) &&
                            fields.contains(OptionalLabel("nickname"))
                        case _                   => false
                    }
                )
            },
            test("imports namespace-prefixed element ref") {
                // Schema with targetNamespace and prefixed refs
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    |           xmlns:tns="http://example.com/test"
                    |           targetNamespace="http://example.com/test">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element ref="tns:child"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |  <xs:element name="child" type="xs:string"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("child")) &&
                            fields.get(MandatoryLabel("child")).exists(_.isInstanceOf[TextValue])
                        case _                   => false
                    }
                )
            },
            test("imports namespace-prefixed group ref") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    |           xmlns:tns="http://example.com/test"
                    |           targetNamespace="http://example.com/test">
                    |  <xs:group name="nameGroup">
                    |    <xs:sequence>
                    |      <xs:element name="firstName" type="xs:string"/>
                    |      <xs:element name="lastName" type="xs:string"/>
                    |    </xs:sequence>
                    |  </xs:group>
                    |  <xs:element name="person">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:group ref="tns:nameGroup"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._1).exists {
                        case ObjectValue(fields) =>
                            fields.contains(MandatoryLabel("firstName")) &&
                            fields.contains(MandatoryLabel("lastName"))
                        case _                   => false
                    }
                )
            },
            test("reports friction for external namespace ref") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    |           xmlns:ext="http://external.example.com"
                    |           targetNamespace="http://example.com/test">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element ref="ext:externalElement"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.map(_._2.nonEmpty) == Right(true),
                    result.exists { case (_, friction) =>
                        friction.entries.exists(_.message.contains("not found"))
                    }
                )
            }
        ),
        suite("Import and Include")(
            test("reports friction for import without fetcher") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    |           xmlns:ext="http://example.com/ext"
                    |           targetNamespace="http://example.com/main">
                    |  <xs:import namespace="http://example.com/ext" schemaLocation="ext.xsd"/>
                    |  <xs:element name="root" type="xs:string"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (_, friction) =>
                        friction.entries.exists(_.message.contains("Failed to fetch"))
                    }
                )
            },
            test("imports schema with fetcher") {
                val mainXsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:import schemaLocation="types.xsd"/>
                    |  <xs:element name="root" type="customType"/>
                    |</xs:schema>""".stripMargin

                val typesXsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:complexType name="customType">
                    |    <xs:sequence>
                    |      <xs:element name="imported" type="xs:string"/>
                    |    </xs:sequence>
                    |  </xs:complexType>
                    |</xs:schema>""".stripMargin

                val fetcher = new SchemaFetcher:
                    def fetch(url: String): Either[String, String] =
                        if url == "types.xsd" then Right(typesXsd)
                        else Left(s"Unknown URL: $url")

                val result = XsdImporter.fromXsd(mainXsd, fetcher)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, _) =>
                        schema match
                            case ObjectValue(fields) =>
                                fields.exists { case (l, _) => l.label == "imported" }
                            case _                   => false
                    }
                )
            },
            test("imports cross-namespace attribute ref") {
                val mainXsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    |           xmlns:xml="http://www.w3.org/XML/1998/namespace"
                    |           targetNamespace="http://example.com/main">
                    |  <xs:import namespace="http://www.w3.org/XML/1998/namespace" schemaLocation="xml.xsd"/>
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:attribute ref="xml:lang"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val xmlXsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    |           targetNamespace="http://www.w3.org/XML/1998/namespace">
                    |  <xs:attribute name="lang" type="xs:string"/>
                    |</xs:schema>""".stripMargin

                val fetcher = new SchemaFetcher:
                    def fetch(url: String): Either[String, String] =
                        if url == "xml.xsd" then Right(xmlXsd)
                        else Left(s"Unknown URL: $url")

                val result = XsdImporter.fromXsd(mainXsd, fetcher)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, friction) =>
                        schema match
                            case ObjectValue(fields) =>
                                // Should have @lang attribute from imported namespace
                                fields.exists { case (l, _) => l.label == "@lang" }
                            case _                   => false
                    },
                    // No friction about "not found" since it was resolved from import
                    result.exists { case (_, friction) =>
                        !friction.entries.exists(_.message.contains("xml:lang"))
                    }
                )
            },
            test("handles include for same namespace") {
                val mainXsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    |           targetNamespace="http://example.com/main">
                    |  <xs:include schemaLocation="common.xsd"/>
                    |  <xs:element name="root" type="sharedType"/>
                    |</xs:schema>""".stripMargin

                val commonXsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
                    |           targetNamespace="http://example.com/main">
                    |  <xs:complexType name="sharedType">
                    |    <xs:sequence>
                    |      <xs:element name="shared" type="xs:integer"/>
                    |    </xs:sequence>
                    |  </xs:complexType>
                    |</xs:schema>""".stripMargin

                val fetcher = new SchemaFetcher:
                    def fetch(url: String): Either[String, String] =
                        if url == "common.xsd" then Right(commonXsd)
                        else Left(s"Unknown URL: $url")

                val result = XsdImporter.fromXsd(mainXsd, fetcher)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, _) =>
                        schema match
                            case ObjectValue(fields) =>
                                fields.exists { case (l, _) => l.label == "shared" }
                            case _                   => false
                    }
                )
            },
            test("reports friction for import without schemaLocation") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:import namespace="http://example.com/ext"/>
                    |  <xs:element name="root" type="xs:string"/>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (_, friction) =>
                        friction.entries.exists(_.message.contains("no schemaLocation"))
                    }
                )
            }
        ),
        suite("Complex Content")(
            test("imports complexContent extension with sequence") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:complexType name="baseType">
                    |    <xs:sequence>
                    |      <xs:element name="baseField" type="xs:string"/>
                    |    </xs:sequence>
                    |  </xs:complexType>
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:complexContent>
                    |        <xs:extension base="baseType">
                    |          <xs:sequence>
                    |            <xs:element name="extField" type="xs:integer"/>
                    |          </xs:sequence>
                    |        </xs:extension>
                    |      </xs:complexContent>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, _) =>
                        schema match
                            case ObjectValue(fields) =>
                                fields.exists { case (l, _) => l.label == "baseField" } &&
                                fields.exists { case (l, _) => l.label == "extField" }
                            case _                   => false
                    }
                )
            },
            test("imports complexContent extension with attributes") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:complexType name="baseType">
                    |    <xs:sequence>
                    |      <xs:element name="content" type="xs:string"/>
                    |    </xs:sequence>
                    |    <xs:attribute name="baseAttr" type="xs:string"/>
                    |  </xs:complexType>
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:complexContent>
                    |        <xs:extension base="baseType">
                    |          <xs:attribute name="extAttr" type="xs:integer"/>
                    |        </xs:extension>
                    |      </xs:complexContent>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, _) =>
                        schema match
                            case ObjectValue(fields) =>
                                fields.exists { case (l, _) => l.label == "@extAttr" }
                            case _                   => false
                    }
                )
            },
            test("imports complexContent restriction") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:complexType name="baseType">
                    |    <xs:sequence>
                    |      <xs:element name="field1" type="xs:string"/>
                    |      <xs:element name="field2" type="xs:string"/>
                    |    </xs:sequence>
                    |  </xs:complexType>
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:complexContent>
                    |        <xs:restriction base="baseType">
                    |          <xs:sequence>
                    |            <xs:element name="field1" type="xs:string"/>
                    |          </xs:sequence>
                    |        </xs:restriction>
                    |      </xs:complexContent>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, friction) =>
                        schema.isInstanceOf[ObjectValue] &&
                        friction.entries.exists(_.message.contains("restriction"))
                    }
                )
            }
        ),
        suite("Mixed Content")(
            test("imports mixed content type") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType mixed="true">
                    |      <xs:sequence>
                    |        <xs:element name="bold" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
                    |        <xs:element name="italic" type="xs:string" minOccurs="0" maxOccurs="unbounded"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, friction) =>
                        schema match
                            case ObjectValue(fields) =>
                                fields.exists { case (l, _) => l.label == "_text" } &&
                                fields.exists { case (l, _) => l.label == "bold" } &&
                                friction.entries.exists(_.message.contains("mixed content"))
                            case _                   => false
                    }
                )
            },
            test("imports mixed content with attributes") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType mixed="true">
                    |      <xs:sequence>
                    |        <xs:element name="child" type="xs:string" minOccurs="0"/>
                    |      </xs:sequence>
                    |      <xs:attribute name="id" type="xs:string"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, _) =>
                        schema match
                            case ObjectValue(fields) =>
                                fields.exists { case (l, _) => l.label == "_text" } &&
                                fields.exists { case (l, _) => l.label == "@id" } &&
                                fields.exists { case (l, _) => l.label == "child" }
                            case _                   => false
                    }
                )
            }
        ),
        suite("Wildcards")(
            test("imports xs:any in sequence") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="known" type="xs:string"/>
                    |        <xs:any minOccurs="0" maxOccurs="unbounded" processContents="lax"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    // Should produce a TupleValue since xs:any is unnamed
                    result.exists { case (schema, friction) =>
                        schema.isInstanceOf[TupleValue] &&
                        friction.entries.exists(_.message.contains("xs:any"))
                    }
                )
            },
            test("imports xs:any with occurrence constraints") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:any minOccurs="1" maxOccurs="5" processContents="strict"/>
                    |      </xs:sequence>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, _) =>
                        schema match
                            case TupleValue(ListOfValues(MapValue(_), _)) => true
                            case _                                        => false
                    }
                )
            },
            test("imports xs:any in choice") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:choice>
                    |        <xs:element name="option1" type="xs:string"/>
                    |        <xs:any namespace="##other" processContents="lax"/>
                    |      </xs:choice>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, _) =>
                        schema.isInstanceOf[AlternativeValues]
                    }
                )
            },
            test("imports xs:anyAttribute") {
                val xsd = """<?xml version="1.0"?>
                    |<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
                    |  <xs:element name="root">
                    |    <xs:complexType>
                    |      <xs:sequence>
                    |        <xs:element name="child" type="xs:string"/>
                    |      </xs:sequence>
                    |      <xs:anyAttribute namespace="##other" processContents="lax"/>
                    |    </xs:complexType>
                    |  </xs:element>
                    |</xs:schema>""".stripMargin

                val result = XsdImporter.fromXsd(xsd)
                assertTrue(
                    result.isRight,
                    result.exists { case (schema, friction) =>
                        schema.isInstanceOf[ObjectValue] &&
                        friction.entries.exists(_.message.contains("xs:anyAttribute"))
                    }
                )
            }
        )
    )

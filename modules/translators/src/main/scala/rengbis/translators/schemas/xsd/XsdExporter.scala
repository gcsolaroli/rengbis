package rengbis.translators.schemas.xsd

import rengbis.Schema.*
import rengbis.translators.common.*
import scala.xml.{ Elem, Node, NodeSeq, Null, PrettyPrinter, Text, UnprefixedAttribute }

/** Exports ReNGBis schemas to XML Schema Definition (XSD) format.
  *
  * This exporter translates ReNGBis schemas to XSD 1.0. Since XSD has different semantics and capabilities, some features may not translate perfectly. The FrictionReport tracks any translation issues.
  *
  * Key design decisions:
  *   - Object fields are exported as xs:element children (not attributes by default)
  *   - Named schemas become global xs:complexType or xs:simpleType definitions
  *   - The `_text` field indicates simple content with attributes pattern
  *   - Ordered sequences use xs:sequence, alternatives use xs:choice
  *
  * Note: This implementation builds XML programmatically (Scala 3 doesn't support XML literals)
  */
object XsdExporter:

    private val XS_NAMESPACE = "http://www.w3.org/2001/XMLSchema"
    private val XS_PREFIX    = "xs"

    /** Exports a ReNGBis schema to XSD.
      *
      * @param schema
      *   The ReNGBis schema to export
      * @param rootElementName
      *   The name for the root element (default: "root")
      * @return
      *   A tuple of (XSD as XML string, FrictionReport)
      */
    def toXsd(schema: Schema, rootElementName: String = "root"): (String, FrictionReport) =
        val context = TranslationContext()

        // Translate the root schema to type content
        val (rootTypeContent, rootContext) = translateSchema(schema, context.atPath(rootElementName))

        // Build the root complex type
        val rootType = elem("complexType", attr("name", "RootType"), rootTypeContent)

        // Build the root element
        val rootElement = elem("element", attr("name", rootElementName, attr("type", "RootType")), Seq.empty)

        // Build the complete XSD schema
        val schemaElem = elem("schema", attr("xmlns:xs", XS_NAMESPACE, attr("elementFormDefault", "qualified")), Seq(rootElement, rootType))

        val printer = new PrettyPrinter(80, 2)
        (printer.format(schemaElem), rootContext.report)

    private case class TranslationContext(
        path: String = "$",
        report: FrictionReport = FrictionReport()
    ):
        def atPath(newPath: String): TranslationContext =
            copy(path = if newPath.startsWith("$") then newPath else s"$path/$newPath")

        def addLoss(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addLoss(path, message, suggestion))

        def addApproximation(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addApproximation(path, message, suggestion))

        def addExtension(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addExtension(path, message, suggestion))

    // Helper methods for building XML elements programmatically
    private def elem(name: String, attributes: scala.xml.MetaData, children: Seq[Node]): Elem =
        Elem(XS_PREFIX, name, attributes, xml.TopScope, minimizeEmpty = true, children*)

    private def elem(name: String, attributes: scala.xml.MetaData, child: NodeSeq): Elem =
        Elem(XS_PREFIX, name, attributes, xml.TopScope, minimizeEmpty = true, child.toSeq*)

    private def elem(name: String, attributes: scala.xml.MetaData): Elem =
        Elem(XS_PREFIX, name, attributes, xml.TopScope, minimizeEmpty = true)

    private def elem(name: String, children: Seq[Node]): Elem =
        elem(name, scala.xml.Null, children)

    private def elem(name: String, child: NodeSeq): Elem =
        elem(name, scala.xml.Null, child)

    private def elem(name: String): Elem =
        elem(name, scala.xml.Null, Seq.empty)

    private def attr(name: String, value: String, next: scala.xml.MetaData = scala.xml.Null): scala.xml.MetaData =
        new UnprefixedAttribute(name, value, next)

    private def text(content: String): Text =
        Text(content)

    private def annotation(doc: String): Node =
        elem("annotation", elem("documentation", Seq(text(doc))))

    /** Translates a schema to XML content */
    private def translateSchema(schema: Schema, context: TranslationContext): (NodeSeq, TranslationContext) =
        schema match
            // Wrapper schemas
            case Documented(doc, innerSchema) =>
                val (innerXml, newContext) = translateSchema(innerSchema, context)
                val withAnnotation         = doc match
                    case Some(d) => annotation(d) +: innerXml
                    case None    => innerXml
                (withAnnotation, newContext)

            case Deprecated(innerSchema) =>
                val (innerXml, newContext) = translateSchema(innerSchema, context)
                val withAnnotation         = annotation("DEPRECATED") +: innerXml
                (withAnnotation, newContext)

            // Basic types - simple types
            case TextValue(constraints, default) =>
                translateTextValue(constraints, default, context)

            case GivenTextValue(value) =>
                val restriction = elem("restriction", attr("base", "xs:string"), Seq(elem("enumeration", attr("base", value))))
                (Seq(restriction), context)

            case NumericValue(constraints, default) =>
                translateNumericValue(constraints, default, context)

            case TimeValue(constraints @ _*) =>
                translateTimeValue(constraints, context)

            case BooleanValue(_) =>
                val restriction = elem("restriction", attr("base", "xs:boolean"))
                (Seq(restriction), context)

            case AnyValue() =>
                val complexType = elem("complexType", attr("mixed", "true"), elem("sequence", elem("any", attr("minOccurs", "0", attr("maxOccurs", "unbounded", attr("processContents", "skip"))))))
                (Seq(complexType), context)

            case Fail() =>
                val newContext  = context.addLoss(
                    "XSD does not support 'fail' schema",
                    Some("Using xs:restriction base='xs:string' with impossible pattern")
                )
                val restriction = elem("restriction", attr("base", "xs:string"), elem("pattern", attr("value", "(?!)")))
                (Seq(restriction), newContext)

            // Container types
            case ListOfValues(elementSchema, constraints) =>
                translateListOfValues(elementSchema, constraints, context)

            case TupleValue(elementSchemas @ _*) =>
                translateTupleValue(elementSchemas, context)

            case ObjectValue(obj) =>
                translateObjectValue(obj, context)

            case MapValue(valueSchema) =>
                translateMapValue(valueSchema, context)

            case EnumValues(values @ _*) =>
                translateEnum(values, context)

            case AlternativeValues(schemas @ _*) =>
                translateAlternatives(schemas, context)

            case NamedValueReference(name) =>
                val complexType = elem("complexType", elem("complexContent", elem("extension", attr("base", name))))
                (Seq(complexType), context)

            case ImportStatement(namespace, path) =>
                val newContext = context.addApproximation(
                    s"Import statements converted to xs:include",
                    Some(s"Add: <xs:include schemaLocation='$path'/>")
                )
                (Seq(annotation(s"Imported from: $path")), newContext)

            case _ =>
                val newContext = context.addLoss(s"Unsupported schema type: ${ schema.getClass.getSimpleName }")
                (Seq(elem("any")), newContext)

    private def translateTextValue(constraints: TextConstraint.Constraints, default: Option[String], context: TranslationContext): (NodeSeq, TranslationContext) =
        var baseType   = "xs:string"
        var facets     = List.empty[Node]
        var newContext = context

        // Handle size constraints
        constraints.size.foreach { sizeRange =>
            sizeRange.min.foreach { bound =>
                if bound.isExact then facets = facets :+ elem("length", attr("value", bound.value.toString))
                else if bound.isMinInclusive then facets = facets :+ elem("minLength", attr("value", bound.value.toString))
            }
            sizeRange.max.foreach { bound =>
                if bound.isMaxInclusive then facets = facets :+ elem("maxLength", attr("value", bound.value.toString))
            }
        }

        // Handle regex
        constraints.regex.foreach { pattern =>
            facets = facets :+ elem("pattern", attr("value", pattern))
        }

        // Handle format
        constraints.format.foreach { format =>
            format match
                case "email"            =>
                    facets = facets :+ elem("pattern", attr("value", "[^@]+@[^@]+\\.[^@]+"))
                case "uri"              =>
                    baseType = "xs:anyURI"
                case "iso8601-date"     =>
                    baseType = "xs:date"
                case "iso8601-time"     =>
                    baseType = "xs:time"
                case "iso8601-datetime" =>
                    baseType = "xs:dateTime"
                case other              =>
                    newContext = newContext.addLoss(
                        s"Custom format '$other' not supported in XSD",
                        Some("Consider using xs:pattern restriction")
                    )
        }

        default.foreach { d =>
            facets = facets :+ annotation(s"Default: $d")
        }

        val restriction = elem("restriction", attr("base", baseType), facets)
        (Seq(restriction), newContext)

    private def translateNumericValue(constraints: NumericConstraint.Constraints, default: Option[BigDecimal], context: TranslationContext): (NodeSeq, TranslationContext) =
        var baseType   = if constraints.integer then "xs:integer" else "xs:decimal"
        var facets     = List.empty[Node]
        var newContext = context

        // Handle value constraints
        constraints.value.foreach { valueRange =>
            valueRange.min.foreach { bound =>
                if bound.isExact then
                    facets = facets :+ elem("minInclusive", attr("value", bound.value.toString))
                    facets = facets :+ elem("maxInclusive", attr("value", bound.value.toString))
                else if bound.isMinInclusive then facets = facets :+ elem("minInclusive", attr("value", bound.value.toString))
                else if bound.isMinExclusive then
                    newContext = newContext.addApproximation(
                        s"Exclusive minimum not directly supported in XSD",
                        Some("Consider using minInclusive with value + epsilon")
                    )
            }
            valueRange.max.foreach { bound =>
                if bound.isMaxInclusive then facets = facets :+ elem("maxInclusive", attr("value", bound.value.toString))
                else if bound.isMaxExclusive then
                    newContext = newContext.addApproximation(
                        s"Exclusive maximum not directly supported in XSD",
                        Some("Consider using maxInclusive with value - epsilon")
                    )
            }
        }

        default.foreach { d =>
            facets = facets :+ annotation(s"Default: $d")
        }

        val restriction = elem("restriction", attr("base", baseType), facets)
        (Seq(restriction), newContext)

    private def translateTimeValue(constraints: Seq[TimeConstraint.Constraint], context: TranslationContext): (NodeSeq, TranslationContext) =
        val restriction = elem("restriction", attr("base", "xs:dateTime"))

        val newContext =
            if constraints.nonEmpty then
                context.addLoss(
                    "Time constraints not fully supported in XSD",
                    Some("Using xs:dateTime as base type")
                )
            else context

        (Seq(restriction), newContext)

    private def translateEnum(values: Seq[String], context: TranslationContext): (NodeSeq, TranslationContext) =
        val enumerations = values.map(v => elem("enumeration", attr("value", v)))
        val restriction  = elem("restriction", attr("base", "xs:string"), enumerations)
        val simpleType   = elem("simpleType", Seq(restriction))
        (Seq(simpleType), context)

    private def translateListOfValues(elementSchema: Schema, constraints: ListConstraint.Constraints, context: TranslationContext): (NodeSeq, TranslationContext) =
        val (elementType, elemContext) = translateSchema(elementSchema, context.atPath("items"))

        var minOccurs  = "0"
        var maxOccurs  = "unbounded"
        var newContext = elemContext

        // Handle size constraints
        constraints.size.foreach { sizeRange =>
            sizeRange.min.foreach { bound =>
                if bound.isExact then
                    minOccurs = bound.value.toString
                    maxOccurs = bound.value.toString
                else if bound.isMinInclusive then minOccurs = bound.value.toString
            }
            sizeRange.max.foreach { bound =>
                if bound.isMaxInclusive then maxOccurs = bound.value.toString
            }
        }

        // Handle uniqueness constraints
        constraints.unique.foreach:
            case ListConstraint.Uniqueness.Simple      =>
                newContext = newContext.addApproximation(
                    "UniqueItems constraint not directly supported in XSD",
                    Some("Consider using xs:unique constraint")
                )
            case ListConstraint.Uniqueness.ByFields(_) =>
                newContext = newContext.addLoss(
                    "UniqueByFields constraint not supported in XSD",
                    Some("Consider using xs:unique or xs:key")
                )

        val itemElement = elem("element", attr("name", "item", attr("minOccurs", minOccurs, attr("maxOccurs", maxOccurs))), elementType)

        val sequence = elem("sequence", Seq(itemElement))
        (Seq(sequence), newContext)

    private def translateTupleValue(elementSchemas: Seq[Schema], context: TranslationContext): (NodeSeq, TranslationContext) =
        val (elements, finalContext) = elementSchemas.zipWithIndex.foldLeft((List.empty[Node], context)) { case ((elems, ctx), (elemSchema, idx)) =>
            val (elemType, newCtx) = translateSchema(elemSchema, ctx.atPath(s"item$idx"))
            val elem               = this.elem("element", attr("name", s"item$idx", attr("minOccurs", "1", attr("maxOccurs", "1"))), elemType)
            (elems :+ elem, newCtx)
        }

        val sequence = elem("sequence", elements)
        (Seq(sequence), finalContext)

    private def translateObjectValue(obj: Map[ObjectLabel, Schema], context: TranslationContext): (NodeSeq, TranslationContext) =
        val (elements, finalContext) = obj.foldLeft((List.empty[Node], context)) { case ((elems, ctx), (label, fieldSchema)) =>
            val fieldName           = label.label
            val isOptional          = label.isInstanceOf[OptionalLabel]
            val (fieldType, newCtx) = translateSchema(fieldSchema, ctx.atPath(fieldName))

            val elemNode = this.elem("element", attr("name", fieldName, attr("minOccurs", if isOptional then "0" else "1", attr("maxOccurs", "1"))), fieldType)
            (elems :+ elemNode, newCtx)
        }

        val sequence = elem("sequence", elements)
        (Seq(sequence), finalContext)

    private def translateMapValue(valueSchema: Schema, context: TranslationContext): (NodeSeq, TranslationContext) =
        val (valueType, newContext) = translateSchema(valueSchema, context.atPath("additionalProperties"))

        val approxContext = newContext.addApproximation(
            "Map type approximated as sequence of key-value pairs",
            Some("Consider defining explicit key-value pair element")
        )

        val valueElement = elem("element", attr("name", "value"), valueType)

        val keyElement = elem("element", attr("name", "key", attr("type", "xs:string")))

        val entrySequence    = elem("sequence", Seq(keyElement, valueElement))
        val entryComplexType = elem("complexType", Seq(entrySequence))

        val entryElement = elem("element", attr("name", "entry", attr("minOccurs", "0", attr("maxOccurs", "unbounded"))), Seq(entryComplexType))

        val sequence = elem("sequence", Seq(entryElement))
        (Seq(sequence), approxContext)

    private def translateAlternatives(schemas: Seq[Schema], context: TranslationContext): (NodeSeq, TranslationContext) =
        val (choices, finalContext) = schemas.zipWithIndex.foldLeft((List.empty[Node], context)) { case ((elems, ctx), (altSchema, idx)) =>
            val (altType, newCtx) = translateSchema(altSchema, ctx.atPath(s"choice$idx"))
            val choiceElem        = this.elem("element", attr("name", s"choice$idx"), altType)
            (elems :+ choiceElem, newCtx)
        }

        val choice = elem("choice", choices)
        (Seq(choice), finalContext)

package rengbis.translators.schemas.xsd

import rengbis.Schema.*
import rengbis.translators.common.*
import scala.xml.{ Elem, Node, NodeSeq, XML }
import scala.util.{ Failure, Success, Try }

/** Imports XML Schema Definition (XSD) to ReNGBis schemas.
  *
  * This importer translates XSD 1.0 schemas to ReNGBis schemas. Since XSD has different semantics and capabilities, some features may not translate perfectly. The FrictionReport tracks any translation issues.
  *
  * Key design decisions:
  *   - xs:element children become object fields
  *   - xs:attribute elements are converted to fields with @ prefix
  *   - xs:sequence becomes ObjectValue or TupleValue depending on whether elements are named
  *   - xs:choice becomes AlternativeValues
  *   - Named types (xs:complexType, xs:simpleType) become references
  */
object XsdImporter:

    private val XS_NAMESPACE = "http://www.w3.org/2001/XMLSchema"

    /** Imports an XSD schema to ReNGBis schema.
      *
      * @param xsdText
      *   The XSD schema as a string
      * @return
      *   Either an error message or tuple of (ReNGBis Schema, FrictionReport)
      */
    def fromXsd(xsdText: String): Either[String, (Schema, FrictionReport)] =
        Try(XML.loadString(xsdText)) match
            case Failure(exception)  => Left(s"Failed to parse XML: ${ exception.getMessage }")
            case Success(schemaElem) =>
                if schemaElem.label != "schema" then Left(s"Expected xs:schema root element, got ${ schemaElem.label }")
                else
                    val context = TranslationContext()

                    // Extract global type definitions
                    val globalTypes      = extractGlobalTypes(schemaElem)
                    val contextWithTypes = context.withTypes(globalTypes)

                    // Find the root element
                    val rootElements = (schemaElem \ "element").filter(_.prefix == "xs")
                    rootElements.headOption match
                        case None           => Left("No root element found in XSD schema")
                        case Some(rootElem) =>
                            translateElement(rootElem, contextWithTypes) match
                                case Left(error)          => Left(error)
                                case Right((schema, ctx)) => Right((schema, ctx.report))

    private case class TranslationContext(
        path: String = "$",
        report: FrictionReport = FrictionReport(),
        types: Map[String, Node] = Map.empty
    ):
        def atPath(newPath: String): TranslationContext =
            copy(path = if newPath.startsWith("$") then newPath else s"$path/$newPath")

        def addLoss(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addLoss(path, message, suggestion))

        def addApproximation(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addApproximation(path, message, suggestion))

        def addExtension(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addExtension(path, message, suggestion))

        def withTypes(newTypes: Map[String, Node]): TranslationContext =
            copy(types = types ++ newTypes)

    /** Extracts global type definitions (complexType and simpleType) from the schema */
    private def extractGlobalTypes(schemaElem: Elem): Map[String, Node] =
        val complexTypes = (schemaElem \ "complexType").filter(_.prefix == "xs").flatMap { node =>
            (node \ "@name").headOption.map(_.text -> node)
        }
        val simpleTypes  = (schemaElem \ "simpleType").filter(_.prefix == "xs").flatMap { node =>
            (node \ "@name").headOption.map(_.text -> node)
        }
        (complexTypes ++ simpleTypes).toMap

    private def translateElement(elem: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val name      = (elem \ "@name").text
        val typeName  = (elem \ "@type").text
        val minOccurs = (elem \ "@minOccurs").headOption.map(_.text).getOrElse("1")
        val maxOccurs = (elem \ "@maxOccurs").headOption.map(_.text).getOrElse("1")

        // Check if element has inline type definition
        val typeResult =
            if typeName.nonEmpty then translateTypeReference(typeName, context)
            else
                // Inline type definition
                val inlineTypes = (elem \ "complexType") ++ (elem \ "simpleType")
                inlineTypes.headOption match
                    case Some(inlineType) => translateType(inlineType, context.atPath(name))
                    case None             => Right((TextValue(), context)) // Default to text if no type specified

        typeResult.flatMap { case (schema, ctx) =>
            // Handle occurrence constraints
            if maxOccurs == "unbounded" || maxOccurs.toIntOption.exists(_ > 1) then
                val listConstraints = (minOccurs.toIntOption, maxOccurs) match
                    case (Some(min), "unbounded") if min > 0 =>
                        Seq(ListConstraint.Size(BoundConstraint(BoundOp.MinInclusive, min)))
                    case (Some(min), max)                    =>
                        max.toIntOption match
                            case Some(maxInt) if min > 0 =>
                                Seq(
                                    ListConstraint.Size(BoundConstraint(BoundOp.MinInclusive, min)),
                                    ListConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, maxInt))
                                )
                            case Some(maxInt)            =>
                                Seq(ListConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, maxInt)))
                            case None                    => Seq.empty
                    case _                                   => Seq.empty
                Right((ListOfValues(schema, listConstraints*), ctx))
            else Right((schema, ctx))
        }

    private def translateTypeReference(typeName: String, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        // Check if it's a built-in XSD type
        val baseType = if typeName.startsWith("xs:") then typeName.substring(3) else typeName

        baseType match
            // String types
            case "string" | "normalizedString" | "token" | "language" | "NMTOKEN" | "Name" | "NCName" =>
                Right((TextValue(), context))

            case "ID" | "IDREF" | "ENTITY" =>
                val ctx = context.addLoss(
                    s"XSD type '$baseType' has special semantics not representable in ReNGBis",
                    Some("Using plain TextValue")
                )
                Right((TextValue(), ctx))

            case "IDREFS" | "ENTITIES" | "NMTOKENS" =>
                val ctx = context.addLoss(
                    s"XSD type '$baseType' is a space-separated list with special semantics",
                    Some("Using ListOfValues(TextValue())")
                )
                Right((ListOfValues(TextValue()), ctx))

            // Numeric types
            case "decimal" | "float" | "double" =>
                Right((NumericValue(), context))

            case "integer" | "long" | "int" | "short" | "byte" | "nonNegativeInteger" | "positiveInteger" | "nonPositiveInteger" | "negativeInteger" | "unsignedLong" | "unsignedInt" | "unsignedShort" | "unsignedByte" =>
                val constraints = baseType match
                    case "integer" | "long" | "int" | "short" | "byte"                                            =>
                        Seq(NumericConstraint.Integer)
                    case "nonNegativeInteger" | "unsignedLong" | "unsignedInt" | "unsignedShort" | "unsignedByte" =>
                        Seq(
                            NumericConstraint.Integer,
                            NumericConstraint.Value(BoundConstraint(BoundOp.MinInclusive, BigDecimal(0)))
                        )
                    case "positiveInteger"                                                                        =>
                        Seq(
                            NumericConstraint.Integer,
                            NumericConstraint.Value(BoundConstraint(BoundOp.MinInclusive, BigDecimal(1)))
                        )
                    case "nonPositiveInteger"                                                                     =>
                        Seq(
                            NumericConstraint.Integer,
                            NumericConstraint.Value(BoundConstraint(BoundOp.MaxInclusive, BigDecimal(0)))
                        )
                    case "negativeInteger"                                                                        =>
                        Seq(
                            NumericConstraint.Integer,
                            NumericConstraint.Value(BoundConstraint(BoundOp.MaxInclusive, BigDecimal(-1)))
                        )
                    case _                                                                                        => Seq(NumericConstraint.Integer)
                Right((NumericValue(constraints), context))

            // Date/Time types
            case "dateTime" | "dateTimeStamp" =>
                Right((TextValue(Seq(TextConstraint.Format("iso8601-datetime"))), context))

            case "date" =>
                Right((TextValue(Seq(TextConstraint.Format("iso8601-date"))), context))

            case "time" =>
                Right((TextValue(Seq(TextConstraint.Format("iso8601-time"))), context))

            case "duration" | "dayTimeDuration" | "yearMonthDuration" | "gYear" | "gYearMonth" | "gMonth" | "gMonthDay" | "gDay" =>
                val ctx = context.addLoss(
                    s"XSD type '$baseType' has no direct ReNGBis equivalent",
                    Some("Using TextValue with pattern constraint")
                )
                Right((TextValue(), ctx))

            // Boolean
            case "boolean" =>
                Right((BooleanValue(), context))

            // Binary types
            case "hexBinary" | "base64Binary" =>
                val ctx = context.addApproximation(
                    s"XSD type '$baseType' represented as TextValue",
                    Some("Consider using BinaryValue if binary support is added")
                )
                Right((TextValue(), ctx))

            // URI
            case "anyURI" =>
                Right((TextValue(Seq(TextConstraint.Format("uri"))), context))

            // Special types
            case "anyType" | "anySimpleType" | "anyAtomicType" =>
                Right((AnyValue(), context))

            case "QName" | "NOTATION" =>
                val ctx = context.addLoss(
                    s"XSD type '$baseType' has namespace semantics not representable in ReNGBis",
                    Some("Using TextValue")
                )
                Right((TextValue(), ctx))

            // User-defined type reference
            case _ =>
                context.types.get(baseType) match
                    case Some(typeNode) => translateType(typeNode, context.atPath(baseType))
                    case None           => Right((NamedValueReference(baseType), context))

    private def translateType(typeNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        typeNode.label match
            case "simpleType"  => translateSimpleType(typeNode, context)
            case "complexType" => translateComplexType(typeNode, context)
            case _             => Left(s"Unexpected type node: ${ typeNode.label }")

    private def translateSimpleType(simpleTypeNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        // Look for restriction
        val restrictions = (simpleTypeNode \ "restriction").filter(_.prefix == "xs")
        restrictions.headOption match
            case Some(restriction) => translateRestriction(restriction, context)
            case None              =>
                // Check for list or union
                val lists = (simpleTypeNode \ "list").filter(_.prefix == "xs")
                if lists.nonEmpty then
                    val ctx = context.addApproximation(
                        "XSD list type approximated as ListOfValues",
                        Some("Space-separated list semantics are not preserved")
                    )
                    Right((ListOfValues(TextValue()), ctx))
                else
                    val unions = (simpleTypeNode \ "union").filter(_.prefix == "xs")
                    if unions.nonEmpty then
                        val ctx = context.addApproximation(
                            "XSD union type approximated as AlternativeValues",
                            Some("May need manual adjustment")
                        )
                        Right((AnyValue(), ctx))
                    else Right((TextValue(), context))

    private def translateRestriction(restrictionNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val baseType = (restrictionNode \ "@base").text

        // Get the base schema
        translateTypeReference(baseType, context).flatMap { case (baseSchema, ctx) =>
            // Apply facets
            var currentSchema = baseSchema
            var currentCtx    = ctx

            // Enumeration facets
            val enumerations = (restrictionNode \ "enumeration").filter(_.prefix == "xs").map(e => (e \ "@value").text)
            if enumerations.nonEmpty then currentSchema = EnumValues(enumerations*)

            // Pattern facet
            val patterns = (restrictionNode \ "pattern").filter(_.prefix == "xs").map(p => (p \ "@value").text)
            patterns.headOption.foreach { pattern =>
                currentSchema = currentSchema match
                    case TextValue(constraints, default) =>
                        TextValue(constraints :+ TextConstraint.Regex(pattern), default)
                    case other                           => other
            }

            // Length facets
            val length    = (restrictionNode \ "length").filter(_.prefix == "xs").headOption.map(l => (l \ "@value").text.toInt)
            val minLength = (restrictionNode \ "minLength").filter(_.prefix == "xs").headOption.map(l => (l \ "@value").text.toInt)
            val maxLength = (restrictionNode \ "maxLength").filter(_.prefix == "xs").headOption.map(l => (l \ "@value").text.toInt)

            currentSchema = currentSchema match
                case TextValue(constraints, default) =>
                    var newConstraints = constraints
                    length.foreach(len => newConstraints = newConstraints :+ TextConstraint.Size(BoundConstraint(BoundOp.Exact, len)))
                    minLength.foreach(min => newConstraints = newConstraints :+ TextConstraint.Size(BoundConstraint(BoundOp.MinInclusive, min)))
                    maxLength.foreach(max => newConstraints = newConstraints :+ TextConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, max)))
                    TextValue(newConstraints, default)
                case other                           => other

            // Numeric range facets
            val minInclusive = (restrictionNode \ "minInclusive").filter(_.prefix == "xs").headOption.map(m => BigDecimal((m \ "@value").text))
            val maxInclusive = (restrictionNode \ "maxInclusive").filter(_.prefix == "xs").headOption.map(m => BigDecimal((m \ "@value").text))
            val minExclusive = (restrictionNode \ "minExclusive").filter(_.prefix == "xs").headOption.map(m => BigDecimal((m \ "@value").text))
            val maxExclusive = (restrictionNode \ "maxExclusive").filter(_.prefix == "xs").headOption.map(m => BigDecimal((m \ "@value").text))

            currentSchema = currentSchema match
                case NumericValue(constraints, default) =>
                    var newConstraints = constraints
                    minInclusive.foreach(min => newConstraints = newConstraints :+ NumericConstraint.Value(BoundConstraint(BoundOp.MinInclusive, min)))
                    maxInclusive.foreach(max => newConstraints = newConstraints :+ NumericConstraint.Value(BoundConstraint(BoundOp.MaxInclusive, max)))
                    minExclusive.foreach { min =>
                        currentCtx = currentCtx.addApproximation(
                            "XSD minExclusive converted to minInclusive with epsilon adjustment",
                            Some("Consider manual adjustment if precision matters")
                        )
                        newConstraints = newConstraints :+ NumericConstraint.Value(BoundConstraint(BoundOp.MinExclusive, min))
                    }
                    maxExclusive.foreach { max =>
                        currentCtx = currentCtx.addApproximation(
                            "XSD maxExclusive converted to maxInclusive with epsilon adjustment",
                            Some("Consider manual adjustment if precision matters")
                        )
                        newConstraints = newConstraints :+ NumericConstraint.Value(BoundConstraint(BoundOp.MaxExclusive, max))
                    }
                    NumericValue(newConstraints, default)
                case other                              => other

            Right((currentSchema, currentCtx))
        }

    private def translateComplexType(complexTypeNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        // Check for sequence, choice, or all
        val sequences = (complexTypeNode \ "sequence").filter(_.prefix == "xs")
        val choices   = (complexTypeNode \ "choice").filter(_.prefix == "xs")
        val alls      = (complexTypeNode \ "all").filter(_.prefix == "xs")

        if sequences.nonEmpty then translateSequence(sequences.head, context)
        else if choices.nonEmpty then translateChoice(choices.head, context)
        else if alls.nonEmpty then
            val ctx = context.addApproximation(
                "XSD xs:all (unordered elements) converted to xs:sequence",
                Some("Element order is not preserved")
            )
            translateSequence(alls.head, ctx)
        else
            // Simple content or attributes only
            val simpleContents = (complexTypeNode \ "simpleContent").filter(_.prefix == "xs")
            if simpleContents.nonEmpty then
                val ctx = context.addApproximation(
                    "XSD simpleContent with attributes converted to object",
                    Some("Content stored in '_text' field")
                )
                Right((AnyValue(), ctx))
            else Right((AnyValue(), context))

    private def translateSequence(sequenceNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val elements = (sequenceNode \ "element").filter(_.prefix == "xs")

        if elements.isEmpty then Right((AnyValue(), context))
        else
            // Translate all elements
            val results = elements.zipWithIndex.foldLeft[Either[String, (List[(String, Schema, Boolean)], TranslationContext)]](Right((List.empty, context))):
                case (Right((schemas, ctx)), (elem, idx)) =>
                    translateElement(elem, ctx.atPath(s"element[$idx]")) match
                        case Left(error)             => Left(error)
                        case Right((schema, newCtx)) =>
                            val name       = (elem \ "@name").text
                            val minOccurs  = (elem \ "@minOccurs").headOption.map(_.text).getOrElse("1")
                            val isOptional = minOccurs == "0"
                            Right((schemas :+ (name, schema, isOptional), newCtx))
                case (left @ Left(_), _)                  => left

            results.map { case (elements, ctx) =>
                // If all elements are named, create an ObjectValue
                if elements.forall(_._1.nonEmpty) then
                    val fields = elements.map { case (name, schema, isOptional) =>
                        val label = if isOptional then OptionalLabel(name) else MandatoryLabel(name)
                        (label, schema)
                    }.toMap
                    (ObjectValue(fields), ctx)
                else
                    // Otherwise create a TupleValue
                    val schemas = elements.map(_._2)
                    (TupleValue(schemas*), ctx)
            }

    private def translateChoice(choiceNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val elements = (choiceNode \ "element").filter(_.prefix == "xs")

        val results = elements.zipWithIndex.foldLeft[Either[String, (List[Schema], TranslationContext)]](Right((List.empty, context))):
            case (Right((schemas, ctx)), (elem, idx)) =>
                translateElement(elem, ctx.atPath(s"choice[$idx]")) match
                    case Left(error)             => Left(error)
                    case Right((schema, newCtx)) => Right((schemas :+ schema, newCtx))
            case (left @ Left(_), _)                  => left

        results.map { case (schemas, ctx) =>
            (AlternativeValues(schemas*), ctx)
        }

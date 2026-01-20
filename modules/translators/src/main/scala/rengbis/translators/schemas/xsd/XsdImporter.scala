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

    /** Result of importing an XSD schema with definitions */
    case class XsdImportResult(
        definitions: Map[String, Schema],
        report: FrictionReport
    )

    /** Imports an XSD schema to ReNGBis schema with all global elements as named definitions.
      *
      * This method translates all global xs:element declarations into named ReNGBis schemas. This is useful when you want to preserve all top-level elements rather than selecting a single root.
      *
      * @param xsdText
      *   The XSD schema as a string
      * @param fetcher
      *   Optional schema fetcher for resolving xs:import and xs:include
      * @return
      *   Either an error message or XsdImportResult containing all definitions and the friction report
      */
    def fromXsdWithDefinitions(
        xsdText: String,
        fetcher: SchemaFetcher = SchemaFetcher.NoOp
    ): Either[String, XsdImportResult] =
        Try(XML.loadString(xsdText)) match
            case Failure(exception)  => Left(s"Failed to parse XML: ${ exception.getMessage }")
            case Success(schemaElem) =>
                if schemaElem.label != "schema" then Left(s"Expected xs:schema root element, got ${ schemaElem.label }")
                else
                    // Detect the XSD namespace prefix (commonly "xs" or "xsd")
                    val xsPrefix        = detectXsPrefix(schemaElem)
                    val targetNamespace = extractTargetNamespace(schemaElem)
                    val namespaces      = extractNamespaces(schemaElem)
                    val context         = TranslationContext(xsPrefix = xsPrefix, fetcher = fetcher)
                        .withNamespaces(targetNamespace, namespaces)

                    // Process xs:import and xs:include to get imported definitions
                    val (importedContext, importFriction) = processImports(schemaElem, xsPrefix, context)

                    // Extract global definitions from this schema
                    val globalTypes      = extractGlobalTypes(schemaElem, xsPrefix)
                    val globalAttrGroups = extractGlobalAttributeGroups(schemaElem, xsPrefix)
                    val globalGroups     = extractGlobalGroups(schemaElem, xsPrefix)
                    val globalElements   = extractGlobalElements(schemaElem, xsPrefix)
                    val globalAttrs      = extractGlobalAttributes(schemaElem, xsPrefix)

                    // Merge with imported definitions (local takes precedence)
                    val contextWithDefs = importedContext
                        .withTypes(importedContext.types ++ globalTypes)
                        .withAttributeGroups(importedContext.attributeGroups ++ globalAttrGroups)
                        .withGroups(importedContext.groups ++ globalGroups)
                        .withElements(importedContext.elements ++ globalElements)
                        .withAttributes(importedContext.attributes ++ globalAttrs)
                        .copy(report = importFriction)

                    // Translate all global elements into named definitions
                    val rootElements = (schemaElem \ "element").filter(_.prefix == xsPrefix)

                    val result = rootElements.foldLeft[Either[String, (Map[String, Schema], TranslationContext)]](
                        Right((Map.empty, contextWithDefs))
                    ):
                        case (Right((defs, ctx)), elem) =>
                            val name = (elem \ "@name").text
                            if name.nonEmpty then
                                translateElement(elem, ctx.atPath(name)) match
                                    case Left(error)             => Left(error)
                                    case Right((schema, newCtx)) => Right((defs + (name -> schema), newCtx))
                            else Right((defs, ctx)) // Skip elements without names
                        case (left @ Left(_), _)        => left

                    result.map { case (definitions, finalCtx) =>
                        XsdImportResult(definitions, finalCtx.report)
                    }

    /** Imports an XSD schema to ReNGBis schema.
      *
      * @param xsdText
      *   The XSD schema as a string
      * @param fetcher
      *   Optional schema fetcher for resolving xs:import and xs:include
      * @param rootElementName
      *   Optional name of the root element to use. If not specified, uses the first global element.
      * @return
      *   Either an error message or tuple of (ReNGBis Schema, FrictionReport)
      */
    def fromXsd(
        xsdText: String,
        fetcher: SchemaFetcher = SchemaFetcher.NoOp,
        rootElementName: Option[String] = None
    ): Either[String, (Schema, FrictionReport)] =
        Try(XML.loadString(xsdText)) match
            case Failure(exception)  => Left(s"Failed to parse XML: ${ exception.getMessage }")
            case Success(schemaElem) =>
                if schemaElem.label != "schema" then Left(s"Expected xs:schema root element, got ${ schemaElem.label }")
                else
                    // Detect the XSD namespace prefix (commonly "xs" or "xsd")
                    val xsPrefix        = detectXsPrefix(schemaElem)
                    val targetNamespace = extractTargetNamespace(schemaElem)
                    val namespaces      = extractNamespaces(schemaElem)
                    val context         = TranslationContext(xsPrefix = xsPrefix, fetcher = fetcher)
                        .withNamespaces(targetNamespace, namespaces)

                    // Process xs:import and xs:include to get imported definitions
                    val (importedContext, importFriction) = processImports(schemaElem, xsPrefix, context)

                    // Extract global definitions from this schema
                    val globalTypes      = extractGlobalTypes(schemaElem, xsPrefix)
                    val globalAttrGroups = extractGlobalAttributeGroups(schemaElem, xsPrefix)
                    val globalGroups     = extractGlobalGroups(schemaElem, xsPrefix)
                    val globalElements   = extractGlobalElements(schemaElem, xsPrefix)
                    val globalAttrs      = extractGlobalAttributes(schemaElem, xsPrefix)

                    // Merge with imported definitions (local takes precedence)
                    val contextWithDefs = importedContext
                        .withTypes(importedContext.types ++ globalTypes)
                        .withAttributeGroups(importedContext.attributeGroups ++ globalAttrGroups)
                        .withGroups(importedContext.groups ++ globalGroups)
                        .withElements(importedContext.elements ++ globalElements)
                        .withAttributes(importedContext.attributes ++ globalAttrs)
                        .copy(report = importFriction)

                    // Find the root element - either by name or use the first one
                    val rootElements = (schemaElem \ "element").filter(_.prefix == xsPrefix)
                    val rootElemOpt  = rootElementName match
                        case Some(name) =>
                            rootElements.find(e => (e \ "@name").text == name)
                        case None       =>
                            rootElements.headOption

                    rootElemOpt match
                        case None           =>
                            rootElementName match
                                case Some(name) => Left(s"Root element '$name' not found in XSD schema")
                                case None       => Left("No root element found in XSD schema")
                        case Some(rootElem) =>
                            translateElement(rootElem, contextWithDefs) match
                                case Left(error)          => Left(error)
                                case Right((schema, ctx)) => Right((schema, ctx.report))

    /** Detects the namespace prefix used for XML Schema (e.g., "xs" or "xsd") */
    private def detectXsPrefix(schemaElem: Elem): String =
        // The schema element itself uses the XSD prefix, so we can just get its prefix
        // If no prefix (default namespace), return empty string
        Option(schemaElem.prefix).getOrElse("")

    /** Extracts namespace declarations from the schema element */
    private def extractNamespaces(schemaElem: Elem): Map[String, String] =
        // Recursively extract namespace bindings from scope chain
        def extractFromScope(scope: scala.xml.NamespaceBinding, acc: Map[String, String]): Map[String, String] =
            if scope == null || scope == scala.xml.TopScope then acc
            else
                val prefix = Option(scope.prefix).getOrElse("")
                val uri    = Option(scope.uri).getOrElse("")
                val newAcc = if uri.nonEmpty then acc + (prefix -> uri) else acc
                extractFromScope(scope.parent, newAcc)
        extractFromScope(schemaElem.scope, Map.empty)

    /** Extracts the target namespace from the schema element */
    private def extractTargetNamespace(schemaElem: Elem): Option[String] =
        (schemaElem \ "@targetNamespace").headOption.map(_.text)

    /** Extracts global attribute definitions from the schema */
    private def extractGlobalAttributes(schemaElem: Elem, xsPrefix: String): Map[String, Node] =
        (schemaElem \ "attribute")
            .filter(_.prefix == xsPrefix)
            .flatMap { node =>
                (node \ "@name").headOption.map(_.text -> node)
            }
            .toMap

    /** Processes xs:import and xs:include elements to fetch and merge external schema definitions */
    private def processImports(schemaElem: Elem, xsPrefix: String, context: TranslationContext): (TranslationContext, FrictionReport) =
        val imports  = (schemaElem \ "import").filter(_.prefix == xsPrefix)
        val includes = (schemaElem \ "include").filter(_.prefix == xsPrefix)

        var currentContext = context
        var friction       = context.report

        // Process xs:import elements (bring in definitions from another namespace)
        for importNode <- imports do
            val namespace      = (importNode \ "@namespace").headOption.map(_.text)
            val schemaLocation = (importNode \ "@schemaLocation").headOption.map(_.text)

            schemaLocation match
                case Some(location) if !currentContext.importedNamespaces.contains(location) =>
                    // Try to fetch the imported schema
                    context.fetcher.fetch(location) match
                        case Right(importedXsd) =>
                            Try(XML.loadString(importedXsd)) match
                                case Success(importedElem) if importedElem.label == "schema" =>
                                    val importedPrefix  = detectXsPrefix(importedElem)
                                    val importedTypes   = extractGlobalTypes(importedElem, importedPrefix)
                                    val importedGroups  = extractGlobalGroups(importedElem, importedPrefix)
                                    val importedAttrs   = extractGlobalAttributeGroups(importedElem, importedPrefix)
                                    val importedElems   = extractGlobalElements(importedElem, importedPrefix)
                                    val importedAttDefs = extractGlobalAttributes(importedElem, importedPrefix)

                                    // Store definitions both in flat maps (for local namespace lookup)
                                    // and by namespace URI (for cross-namespace lookup)
                                    val importedDefs = ImportedDefinitions(
                                        types = importedTypes,
                                        attributeGroups = importedAttrs,
                                        groups = importedGroups,
                                        elements = importedElems,
                                        attributes = importedAttDefs
                                    )

                                    currentContext = currentContext
                                        .withTypes(importedTypes)
                                        .withGroups(importedGroups)
                                        .withAttributeGroups(importedAttrs)
                                        .withElements(importedElems)
                                        .withAttributes(importedAttDefs)
                                        .copy(importedNamespaces = currentContext.importedNamespaces + location)

                                    // Also store by namespace URI for cross-namespace refs
                                    namespace.foreach { ns =>
                                        currentContext = currentContext.withImportedDefinitions(ns, importedDefs)
                                    }

                                case Success(_) =>
                                    friction = friction.addLoss(
                                        "$",
                                        s"Imported schema at '$location' is not a valid XSD schema",
                                        Some("Ensure the import points to a valid XSD file")
                                    )
                                case Failure(e) =>
                                    friction = friction.addLoss(
                                        "$",
                                        s"Failed to parse imported schema at '$location': ${ e.getMessage }",
                                        Some("Ensure the import points to a valid XML file")
                                    )
                        case Left(error)        =>
                            friction = friction.addLoss(
                                "$",
                                s"Failed to fetch imported schema at '$location': $error",
                                Some("Configure a schema fetcher or provide the schema locally")
                            )
                case Some(location)                                                          =>
                    // Already imported, skip to avoid cycles
                    ()
                case None                                                                    =>
                    // No schemaLocation - can't fetch
                    namespace match
                        case Some(ns) =>
                            friction = friction.addApproximation(
                                "$",
                                s"XSD import for namespace '$ns' has no schemaLocation",
                                Some("Add schemaLocation attribute to enable fetching")
                            )
                        case None     =>
                            friction = friction.addLoss(
                                "$",
                                "XSD import without namespace or schemaLocation",
                                Some("Specify namespace and/or schemaLocation")
                            )

        // Process xs:include elements (bring in definitions from same namespace)
        for includeNode <- includes do
            val schemaLocation = (includeNode \ "@schemaLocation").headOption.map(_.text)

            schemaLocation match
                case Some(location) if !currentContext.importedNamespaces.contains(location) =>
                    context.fetcher.fetch(location) match
                        case Right(includedXsd) =>
                            Try(XML.loadString(includedXsd)) match
                                case Success(includedElem) if includedElem.label == "schema" =>
                                    val includedPrefix  = detectXsPrefix(includedElem)
                                    val includedTypes   = extractGlobalTypes(includedElem, includedPrefix)
                                    val includedGroups  = extractGlobalGroups(includedElem, includedPrefix)
                                    val includedAttrs   = extractGlobalAttributeGroups(includedElem, includedPrefix)
                                    val includedElems   = extractGlobalElements(includedElem, includedPrefix)
                                    val includedAttDefs = extractGlobalAttributes(includedElem, includedPrefix)

                                    currentContext = currentContext
                                        .withTypes(includedTypes)
                                        .withGroups(includedGroups)
                                        .withAttributeGroups(includedAttrs)
                                        .withElements(includedElems)
                                        .withAttributes(includedAttDefs)
                                        .copy(importedNamespaces = currentContext.importedNamespaces + location)

                                case Success(_) =>
                                    friction = friction.addLoss(
                                        "$",
                                        s"Included schema at '$location' is not a valid XSD schema",
                                        Some("Ensure the include points to a valid XSD file")
                                    )
                                case Failure(e) =>
                                    friction = friction.addLoss(
                                        "$",
                                        s"Failed to parse included schema at '$location': ${ e.getMessage }",
                                        Some("Ensure the include points to a valid XML file")
                                    )
                        case Left(error)        =>
                            friction = friction.addLoss(
                                "$",
                                s"Failed to fetch included schema at '$location': $error",
                                Some("Configure a schema fetcher or provide the schema locally")
                            )
                case Some(_)                                                                 =>
                    // Already included, skip
                    ()
                case None                                                                    =>
                    friction = friction.addLoss(
                        "$",
                        "XSD include without schemaLocation",
                        Some("Specify schemaLocation attribute")
                    )

        (currentContext, friction)

    /** Definitions from an imported namespace */
    private case class ImportedDefinitions(
        types: Map[String, Node] = Map.empty,
        attributeGroups: Map[String, Node] = Map.empty,
        groups: Map[String, Node] = Map.empty,
        elements: Map[String, Node] = Map.empty,
        attributes: Map[String, Node] = Map.empty
    ):
        def merge(other: ImportedDefinitions): ImportedDefinitions =
            ImportedDefinitions(
                types ++ other.types,
                attributeGroups ++ other.attributeGroups,
                groups ++ other.groups,
                elements ++ other.elements,
                attributes ++ other.attributes
            )

    private case class TranslationContext(
        path: String = "$",
        report: FrictionReport = FrictionReport(),
        types: Map[String, Node] = Map.empty,
        attributeGroups: Map[String, Node] = Map.empty,
        groups: Map[String, Node] = Map.empty,
        elements: Map[String, Node] = Map.empty,
        attributes: Map[String, Node] = Map.empty,
        xsPrefix: String = "xs",
        targetNamespace: Option[String] = None,
        namespaces: Map[String, String] = Map.empty, // prefix -> namespace URI
        fetcher: SchemaFetcher = SchemaFetcher.NoOp,
        importedNamespaces: Set[String] = Set.empty, // Track already imported schemaLocations to avoid cycles
        importedByNamespace: Map[String, ImportedDefinitions] = Map.empty // namespace URI -> definitions
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

        def withAttributeGroups(newGroups: Map[String, Node]): TranslationContext =
            copy(attributeGroups = attributeGroups ++ newGroups)

        def withGroups(newGroups: Map[String, Node]): TranslationContext =
            copy(groups = groups ++ newGroups)

        def withElements(newElements: Map[String, Node]): TranslationContext =
            copy(elements = elements ++ newElements)

        def withAttributes(newAttrs: Map[String, Node]): TranslationContext =
            copy(attributes = attributes ++ newAttrs)

        def withNamespaces(targetNs: Option[String], nsMap: Map[String, String]): TranslationContext =
            copy(targetNamespace = targetNs, namespaces = nsMap)

        /** Resolves a potentially namespace-prefixed name to a local name. Returns the local name if it's in the target namespace, None if it's external.
          */
        def resolveLocalName(prefixedName: String): Option[String] =
            if prefixedName.contains(":") then
                val parts  = prefixedName.split(":", 2)
                val prefix = parts(0)
                val local  = parts(1)
                // Check if this prefix maps to our target namespace
                namespaces.get(prefix) match
                    case Some(ns) if targetNamespace.contains(ns) => Some(local)
                    case Some(ns) if ns == XS_NAMESPACE           => Some(local) // XSD built-in types
                    case _                                        => None        // External namespace
            else Some(prefixedName) // No prefix, assume local

        /** Resolves a namespace-prefixed name to the namespace URI and local name */
        def resolveNamespaceAndLocalName(prefixedName: String): (Option[String], String) =
            if prefixedName.contains(":") then
                val parts  = prefixedName.split(":", 2)
                val prefix = parts(0)
                val local  = parts(1)
                (namespaces.get(prefix), local)
            else (targetNamespace, prefixedName)

        /** Looks up an attribute by prefixed name, checking both local and imported definitions */
        def lookupAttribute(prefixedName: String): Option[Node] =
            resolveLocalName(prefixedName) match
                case Some(localName) => attributes.get(localName)
                case None            =>
                    // Try imported namespaces
                    val (nsOpt, localName) = resolveNamespaceAndLocalName(prefixedName)
                    nsOpt.flatMap(ns => importedByNamespace.get(ns).flatMap(_.attributes.get(localName)))

        /** Looks up an element by prefixed name, checking both local and imported definitions */
        def lookupElement(prefixedName: String): Option[Node] =
            resolveLocalName(prefixedName) match
                case Some(localName) => elements.get(localName)
                case None            =>
                    // Try imported namespaces
                    val (nsOpt, localName) = resolveNamespaceAndLocalName(prefixedName)
                    nsOpt.flatMap(ns => importedByNamespace.get(ns).flatMap(_.elements.get(localName)))

        /** Looks up a type by prefixed name, checking both local and imported definitions */
        def lookupType(prefixedName: String): Option[Node] =
            resolveLocalName(prefixedName) match
                case Some(localName) => types.get(localName)
                case None            =>
                    // Try imported namespaces
                    val (nsOpt, localName) = resolveNamespaceAndLocalName(prefixedName)
                    nsOpt.flatMap(ns => importedByNamespace.get(ns).flatMap(_.types.get(localName)))

        /** Looks up a group by prefixed name, checking both local and imported definitions */
        def lookupGroup(prefixedName: String): Option[Node] =
            resolveLocalName(prefixedName) match
                case Some(localName) => groups.get(localName)
                case None            =>
                    // Try imported namespaces
                    val (nsOpt, localName) = resolveNamespaceAndLocalName(prefixedName)
                    nsOpt.flatMap(ns => importedByNamespace.get(ns).flatMap(_.groups.get(localName)))

        /** Looks up an attributeGroup by prefixed name, checking both local and imported definitions */
        def lookupAttributeGroup(prefixedName: String): Option[Node] =
            resolveLocalName(prefixedName) match
                case Some(localName) => attributeGroups.get(localName)
                case None            =>
                    // Try imported namespaces
                    val (nsOpt, localName) = resolveNamespaceAndLocalName(prefixedName)
                    nsOpt.flatMap(ns => importedByNamespace.get(ns).flatMap(_.attributeGroups.get(localName)))

        /** Adds imported definitions for a specific namespace */
        def withImportedDefinitions(nsUri: String, defs: ImportedDefinitions): TranslationContext =
            val existing = importedByNamespace.getOrElse(nsUri, ImportedDefinitions())
            copy(importedByNamespace = importedByNamespace + (nsUri -> existing.merge(defs)))

    /** Extracts global type definitions (complexType and simpleType) from the schema */
    private def extractGlobalTypes(schemaElem: Elem, xsPrefix: String): Map[String, Node] =
        val complexTypes = (schemaElem \ "complexType").filter(_.prefix == xsPrefix).flatMap { node =>
            (node \ "@name").headOption.map(_.text -> node)
        }
        val simpleTypes  = (schemaElem \ "simpleType").filter(_.prefix == xsPrefix).flatMap { node =>
            (node \ "@name").headOption.map(_.text -> node)
        }
        (complexTypes ++ simpleTypes).toMap

    /** Extracts global attributeGroup definitions from the schema */
    private def extractGlobalAttributeGroups(schemaElem: Elem, xsPrefix: String): Map[String, Node] =
        (schemaElem \ "attributeGroup")
            .filter(_.prefix == xsPrefix)
            .flatMap { node =>
                (node \ "@name").headOption.map(_.text -> node)
            }
            .toMap

    /** Extracts global group definitions from the schema */
    private def extractGlobalGroups(schemaElem: Elem, xsPrefix: String): Map[String, Node] =
        (schemaElem \ "group")
            .filter(_.prefix == xsPrefix)
            .flatMap { node =>
                (node \ "@name").headOption.map(_.text -> node)
            }
            .toMap

    /** Extracts global element definitions from the schema */
    private def extractGlobalElements(schemaElem: Elem, xsPrefix: String): Map[String, Node] =
        (schemaElem \ "element")
            .filter(_.prefix == xsPrefix)
            .flatMap { node =>
                (node \ "@name").headOption.map(_.text -> node)
            }
            .toMap

    private def translateElement(elem: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val name      = (elem \ "@name").text
        val typeName  = (elem \ "@type").text
        val ref       = (elem \ "@ref").headOption.map(_.text)
        val minOccurs = (elem \ "@minOccurs").headOption.map(_.text).getOrElse("1")
        val maxOccurs = (elem \ "@maxOccurs").headOption.map(_.text).getOrElse("1")

        // Check if element is a reference to a global element
        val typeResult: Either[String, (Schema, TranslationContext)] = ref match
            case Some(refName) =>
                // Use lookupElement which checks both local and imported namespaces
                context.lookupElement(refName) match
                    case Some(refElem) =>
                        // Extract local name for path
                        val localName = if refName.contains(":") then refName.split(":", 2)(1) else refName
                        // Translate the referenced element
                        translateElement(refElem, context.atPath(localName))
                    case None          =>
                        val ctx = context.addLoss(
                            s"XSD element ref='$refName' not found",
                            Some("Define the element or import the schema that defines it")
                        )
                        Right((AnyValue(), ctx))
            case None          =>
                // Check if element has inline type definition
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
                val sizeRange = (minOccurs.toIntOption, maxOccurs) match
                    case (Some(min), "unbounded") if min > 0 =>
                        Some(ListConstraint.SizeRange.minInclusive(min))
                    case (Some(min), max)                    =>
                        max.toIntOption match
                            case Some(maxInt) if min > 0 =>
                                Some(
                                    ListConstraint.SizeRange(
                                        Some(BoundConstraint(BoundOp.MinInclusive, min)),
                                        Some(BoundConstraint(BoundOp.MaxInclusive, maxInt))
                                    )
                                )
                            case Some(maxInt)            =>
                                Some(ListConstraint.SizeRange.maxInclusive(maxInt))
                            case None                    => None
                    case _                                   => None
                Right((ListOfValues(schema, ListConstraint.Constraints(size = sizeRange)), ctx))
            else Right((schema, ctx))
        }

    private def translateTypeReference(typeName: String, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        // Check if it's a built-in XSD type - strip the namespace prefix (xs: or xsd: or the detected prefix)
        val xsPrefix = context.xsPrefix
        // First, try to resolve namespace-prefixed type names to local names
        val baseType = context.resolveLocalName(typeName) match
            case Some(localName) => localName
            case None            =>
                // External namespace - strip prefix for built-in check but keep for error reporting
                if typeName.startsWith(s"$xsPrefix:") then typeName.substring(xsPrefix.length + 1)
                else if typeName.startsWith("xs:") then typeName.substring(3)
                else if typeName.startsWith("xsd:") then typeName.substring(4)
                else typeName

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
                        NumericConstraint.Constraints(integer = true)
                    case "nonNegativeInteger" | "unsignedLong" | "unsignedInt" | "unsignedShort" | "unsignedByte" =>
                        NumericConstraint.Constraints(
                            value = Some(NumericConstraint.ValueRange.minInclusive(BigDecimal(0))),
                            integer = true
                        )
                    case "positiveInteger"                                                                        =>
                        NumericConstraint.Constraints(
                            value = Some(NumericConstraint.ValueRange.minInclusive(BigDecimal(1))),
                            integer = true
                        )
                    case "nonPositiveInteger"                                                                     =>
                        NumericConstraint.Constraints(
                            value = Some(NumericConstraint.ValueRange.maxInclusive(BigDecimal(0))),
                            integer = true
                        )
                    case "negativeInteger"                                                                        =>
                        NumericConstraint.Constraints(
                            value = Some(NumericConstraint.ValueRange.maxInclusive(BigDecimal(-1))),
                            integer = true
                        )
                    case _                                                                                        => NumericConstraint.Constraints(integer = true)
                Right((NumericValue(constraints), context))

            // Date/Time types
            case "dateTime" | "dateTimeStamp" =>
                Right((TextValue(TextConstraint.Constraints(format = Some("iso8601-datetime"))), context))

            case "date" =>
                Right((TextValue(TextConstraint.Constraints(format = Some("iso8601-date"))), context))

            case "time" =>
                Right((TextValue(TextConstraint.Constraints(format = Some("iso8601-time"))), context))

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
                Right((TextValue(TextConstraint.Constraints(format = Some("uri"))), context))

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
        val restrictions = (simpleTypeNode \ "restriction").filter(_.prefix == context.xsPrefix)
        restrictions.headOption match
            case Some(restriction) => translateRestriction(restriction, context)
            case None              =>
                // Check for list or union
                val lists = (simpleTypeNode \ "list").filter(_.prefix == context.xsPrefix)
                if lists.nonEmpty then
                    val ctx = context.addApproximation(
                        "XSD list type approximated as ListOfValues",
                        Some("Space-separated list semantics are not preserved")
                    )
                    Right((ListOfValues(TextValue()), ctx))
                else
                    val unions = (simpleTypeNode \ "union").filter(_.prefix == context.xsPrefix)
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
            val enumerations = (restrictionNode \ "enumeration").filter(_.prefix == context.xsPrefix).map(e => (e \ "@value").text)
            if enumerations.nonEmpty then currentSchema = EnumValues(enumerations*)

            // Pattern facet
            val patterns = (restrictionNode \ "pattern").filter(_.prefix == context.xsPrefix).map(p => (p \ "@value").text)
            patterns.headOption.foreach { pattern =>
                currentSchema = currentSchema match
                    case TextValue(constraints, default) =>
                        TextValue(constraints.copy(regex = Some(pattern)), default)
                    case other                           => other
            }

            // Length facets
            val length    = (restrictionNode \ "length").filter(_.prefix == context.xsPrefix).headOption.map(l => (l \ "@value").text.toInt)
            val minLength = (restrictionNode \ "minLength").filter(_.prefix == context.xsPrefix).headOption.map(l => (l \ "@value").text.toInt)
            val maxLength = (restrictionNode \ "maxLength").filter(_.prefix == context.xsPrefix).headOption.map(l => (l \ "@value").text.toInt)

            currentSchema = currentSchema match
                case TextValue(constraints, default) =>
                    var sizeRange = constraints.size.getOrElse(TextConstraint.SizeRange())
                    length.foreach(len => sizeRange = TextConstraint.SizeRange.exact(len))
                    minLength.foreach(min => sizeRange = sizeRange.merge(TextConstraint.SizeRange.minInclusive(min)))
                    maxLength.foreach(max => sizeRange = sizeRange.merge(TextConstraint.SizeRange.maxInclusive(max)))
                    val newSize   = if sizeRange.isEmpty then None else Some(sizeRange)
                    TextValue(constraints.copy(size = newSize), default)
                case other                           => other

            // Numeric range facets
            val minInclusive = (restrictionNode \ "minInclusive").filter(_.prefix == context.xsPrefix).headOption.map(m => BigDecimal((m \ "@value").text))
            val maxInclusive = (restrictionNode \ "maxInclusive").filter(_.prefix == context.xsPrefix).headOption.map(m => BigDecimal((m \ "@value").text))
            val minExclusive = (restrictionNode \ "minExclusive").filter(_.prefix == context.xsPrefix).headOption.map(m => BigDecimal((m \ "@value").text))
            val maxExclusive = (restrictionNode \ "maxExclusive").filter(_.prefix == context.xsPrefix).headOption.map(m => BigDecimal((m \ "@value").text))

            currentSchema = currentSchema match
                case NumericValue(constraints, default) =>
                    var valueRange = constraints.value.getOrElse(NumericConstraint.ValueRange())
                    minInclusive.foreach(min => valueRange = valueRange.merge(NumericConstraint.ValueRange.minInclusive(min)))
                    maxInclusive.foreach(max => valueRange = valueRange.merge(NumericConstraint.ValueRange.maxInclusive(max)))
                    minExclusive.foreach { min =>
                        currentCtx = currentCtx.addApproximation(
                            "XSD minExclusive converted to minInclusive with epsilon adjustment",
                            Some("Consider manual adjustment if precision matters")
                        )
                        valueRange = valueRange.merge(NumericConstraint.ValueRange.minExclusive(min))
                    }
                    maxExclusive.foreach { max =>
                        currentCtx = currentCtx.addApproximation(
                            "XSD maxExclusive converted to maxInclusive with epsilon adjustment",
                            Some("Consider manual adjustment if precision matters")
                        )
                        valueRange = valueRange.merge(NumericConstraint.ValueRange.maxExclusive(max))
                    }
                    val newValue   = if valueRange.isEmpty then None else Some(valueRange)
                    NumericValue(constraints.copy(value = newValue), default)
                case other                              => other

            Right((currentSchema, currentCtx))
        }

    private def translateComplexType(complexTypeNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        // Check for mixed content
        val isMixed = (complexTypeNode \ "@mixed").headOption.map(_.text).contains("true")

        // Check for sequence, choice, or all
        val sequences       = (complexTypeNode \ "sequence").filter(_.prefix == context.xsPrefix)
        val choices         = (complexTypeNode \ "choice").filter(_.prefix == context.xsPrefix)
        val alls            = (complexTypeNode \ "all").filter(_.prefix == context.xsPrefix)
        val complexContents = (complexTypeNode \ "complexContent").filter(_.prefix == context.xsPrefix)

        // First, get the content model (sequence, choice, all, simpleContent, or complexContent)
        val contentResult: Either[String, (Schema, TranslationContext)] =
            if complexContents.nonEmpty then translateComplexContent(complexContents.head, context)
            else if sequences.nonEmpty then translateSequence(sequences.head, context)
            else if choices.nonEmpty then translateChoice(choices.head, context)
            else if alls.nonEmpty then
                val ctx = context.addApproximation(
                    "XSD xs:all (unordered elements) converted to xs:sequence",
                    Some("Element order is not preserved")
                )
                translateSequence(alls.head, ctx)
            else
                // Simple content or attributes only
                val simpleContents = (complexTypeNode \ "simpleContent").filter(_.prefix == context.xsPrefix)
                if simpleContents.nonEmpty then translateSimpleContent(simpleContents.head, context)
                else Right((ObjectValue(Map.empty), context))

        // Then, extract and merge attributes
        contentResult.flatMap { case (contentSchema, ctx) =>
            translateAttributes(complexTypeNode, ctx).map { case (attrFields, attrCtx) =>
                // Handle mixed content by adding _text field
                val (schemaWithMixed, mixedCtx) =
                    if isMixed then
                        val ctx2 = attrCtx.addApproximation(
                            "XSD mixed content type allows text between elements",
                            Some("Text content stored in '_text' field, may appear multiple times interspersed with elements")
                        )
                        contentSchema match
                            case ObjectValue(fields) =>
                                (ObjectValue(fields + (OptionalLabel("_text") -> ListOfValues(TextValue()))), ctx2)
                            case other               =>
                                // For non-object schemas (like tuple), wrap in object
                                (
                                    ObjectValue(
                                        Map(
                                            MandatoryLabel("_content") -> other,
                                            OptionalLabel("_text")     -> ListOfValues(TextValue())
                                        )
                                    ),
                                    ctx2
                                )
                    else (contentSchema, attrCtx)

                if attrFields.isEmpty then (schemaWithMixed, mixedCtx)
                else
                    // Merge attributes with content schema
                    schemaWithMixed match
                        case ObjectValue(fields) =>
                            (ObjectValue(fields ++ attrFields), mixedCtx)
                        case other               =>
                            // Wrap non-object content in an object with _value field
                            val ctx2   = mixedCtx.addApproximation(
                                "XSD element with both content and attributes wrapped in object",
                                Some("Content stored in '_value' field, attributes prefixed with '@'")
                            )
                            val fields = attrFields + (MandatoryLabel("_value") -> other)
                            (ObjectValue(fields), ctx2)
            }
        }

    /** Translates xs:simpleContent with extension or restriction */
    private def translateSimpleContent(simpleContentNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        // Look for extension or restriction
        val extensions   = (simpleContentNode \ "extension").filter(_.prefix == context.xsPrefix)
        val restrictions = (simpleContentNode \ "restriction").filter(_.prefix == context.xsPrefix)

        if extensions.nonEmpty then
            val extNode  = extensions.head
            val baseType = (extNode \ "@base").text
            translateTypeReference(baseType, context).flatMap { case (baseSchema, ctx) =>
                // Extract attributes from the extension
                translateAttributes(extNode, ctx).map { case (attrFields, attrCtx) =>
                    if attrFields.isEmpty then
                        // Just the simple content
                        (baseSchema, attrCtx)
                    else
                        // Wrap in object with _value for content and @ prefixed attributes
                        val fields = attrFields + (MandatoryLabel("_value") -> baseSchema)
                        (ObjectValue(fields), attrCtx)
                }
            }
        else if restrictions.nonEmpty then
            val restNode = restrictions.head
            translateRestriction(restNode, context).flatMap { case (baseSchema, ctx) =>
                // Extract attributes from the restriction
                translateAttributes(restNode, ctx).map { case (attrFields, attrCtx) =>
                    if attrFields.isEmpty then (baseSchema, attrCtx)
                    else
                        val fields = attrFields + (MandatoryLabel("_value") -> baseSchema)
                        (ObjectValue(fields), attrCtx)
                }
            }
        else
            val ctx = context.addLoss(
                "XSD simpleContent without extension or restriction",
                Some("Using AnyValue")
            )
            Right((AnyValue(), ctx))

    /** Translates xs:complexContent with extension or restriction */
    private def translateComplexContent(complexContentNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val extensions   = (complexContentNode \ "extension").filter(_.prefix == context.xsPrefix)
        val restrictions = (complexContentNode \ "restriction").filter(_.prefix == context.xsPrefix)

        if extensions.nonEmpty then
            val extNode  = extensions.head
            val baseType = (extNode \ "@base").text

            // Translate the base type first
            translateTypeReference(baseType, context).flatMap { case (baseSchema, ctx) =>
                // Extract content model from the extension (sequence, choice, all)
                val sequences = (extNode \ "sequence").filter(_.prefix == context.xsPrefix)
                val choices   = (extNode \ "choice").filter(_.prefix == context.xsPrefix)
                val alls      = (extNode \ "all").filter(_.prefix == context.xsPrefix)

                val extensionContent: Either[String, (Option[Schema], TranslationContext)] =
                    if sequences.nonEmpty then translateSequence(sequences.head, ctx).map { case (s, c) => (Some(s), c) }
                    else if choices.nonEmpty then translateChoice(choices.head, ctx).map { case (s, c) => (Some(s), c) }
                    else if alls.nonEmpty then
                        val allCtx = ctx.addApproximation(
                            "XSD xs:all (unordered elements) converted to xs:sequence",
                            Some("Element order is not preserved")
                        )
                        translateSequence(alls.head, allCtx).map { case (s, c) => (Some(s), c) }
                    else Right((None, ctx))

                extensionContent.flatMap { case (extSchemaOpt, ctx2) =>
                    // Extract attributes from the extension
                    translateAttributes(extNode, ctx2).map { case (attrFields, attrCtx) =>
                        // Merge base schema with extension content and attributes
                        val mergedSchema = (baseSchema, extSchemaOpt) match
                            case (ObjectValue(baseFields), Some(ObjectValue(extFields))) =>
                                // Merge object fields from base and extension
                                ObjectValue(baseFields ++ extFields ++ attrFields)
                            case (ObjectValue(baseFields), None)                         =>
                                // No extension content, just add attributes to base
                                ObjectValue(baseFields ++ attrFields)
                            case (ObjectValue(baseFields), Some(extSchema))              =>
                                // Extension has non-object content - add as special field
                                val ctx3 = attrCtx.addApproximation(
                                    "XSD complexContent extension with non-object content merged into base",
                                    Some("Extension content stored in '_extension' field")
                                )
                                ObjectValue(baseFields ++ attrFields + (MandatoryLabel("_extension") -> extSchema))
                            case (base, Some(ObjectValue(extFields)))                    =>
                                // Base is not an object but extension is - wrap base in _base field
                                val ctx3 = attrCtx.addApproximation(
                                    "XSD complexContent extension with non-object base wrapped",
                                    Some("Base content stored in '_base' field")
                                )
                                ObjectValue(extFields ++ attrFields + (MandatoryLabel("_base") -> base))
                            case (base, None) if attrFields.nonEmpty                     =>
                                // No extension content but has attributes - wrap base
                                ObjectValue(attrFields + (MandatoryLabel("_value") -> base))
                            case (base, None)                                            =>
                                // No extension content, no attributes - just use base
                                base
                            case (base, Some(ext))                                       =>
                                // Both base and extension are non-objects
                                val ctx3 = attrCtx.addApproximation(
                                    "XSD complexContent extension with non-object types merged as tuple",
                                    Some("May need manual adjustment")
                                )
                                TupleValue(base, ext)
                        (mergedSchema, attrCtx)
                    }
                }
            }
        else if restrictions.nonEmpty then
            val restNode = restrictions.head
            val baseType = (restNode \ "@base").text

            // For restriction, we translate the restricted content model, not the base
            // The restriction narrows down what's allowed
            val sequences = (restNode \ "sequence").filter(_.prefix == context.xsPrefix)
            val choices   = (restNode \ "choice").filter(_.prefix == context.xsPrefix)
            val alls      = (restNode \ "all").filter(_.prefix == context.xsPrefix)

            val ctx = context.addApproximation(
                s"XSD complexContent restriction of '$baseType' translated without base type validation",
                Some("Restriction semantics not fully preserved")
            )

            val contentResult: Either[String, (Schema, TranslationContext)] =
                if sequences.nonEmpty then translateSequence(sequences.head, ctx)
                else if choices.nonEmpty then translateChoice(choices.head, ctx)
                else if alls.nonEmpty then
                    val allCtx = ctx.addApproximation(
                        "XSD xs:all (unordered elements) converted to xs:sequence",
                        Some("Element order is not preserved")
                    )
                    translateSequence(alls.head, allCtx)
                else
                    // No content model in restriction - translate base type
                    translateTypeReference(baseType, ctx)

            contentResult.flatMap { case (schema, ctx2) =>
                translateAttributes(restNode, ctx2).map { case (attrFields, attrCtx) =>
                    if attrFields.isEmpty then (schema, attrCtx)
                    else
                        schema match
                            case ObjectValue(fields) => (ObjectValue(fields ++ attrFields), attrCtx)
                            case other               =>
                                (ObjectValue(attrFields + (MandatoryLabel("_value") -> other)), attrCtx)
                }
            }
        else
            val ctx = context.addLoss(
                "XSD complexContent without extension or restriction",
                Some("Using AnyValue")
            )
            Right((AnyValue(), ctx))

    /** Extracts and translates xs:attribute and xs:attributeGroup elements from a node */
    private def translateAttributes(parentNode: Node, context: TranslationContext): Either[String, (Map[ObjectLabel, Schema], TranslationContext)] =
        val attributes      = (parentNode \ "attribute").filter(_.prefix == context.xsPrefix)
        val attributeGroups = (parentNode \ "attributeGroup").filter(_.prefix == context.xsPrefix)
        val anyAttributes   = (parentNode \ "anyAttribute").filter(_.prefix == context.xsPrefix)

        // First, translate individual attributes
        val attrResult = attributes.foldLeft[Either[String, (Map[ObjectLabel, Schema], TranslationContext)]](Right((Map.empty, context))):
            case (Right((fields, ctx)), attrNode) =>
                translateAttribute(attrNode, ctx).map { case (nameOpt, schema, isRequired, newCtx) =>
                    nameOpt match
                        case Some(name) =>
                            val label    = if isRequired then MandatoryLabel(s"@$name") else OptionalLabel(s"@$name")
                            val newField = label -> schema
                            (fields + newField, newCtx)
                        case None       =>
                            // Attribute without name (e.g., ref= not yet supported)
                            (fields, newCtx)
                }
            case (left @ Left(_), _)              => left

        // Then, expand attributeGroup references
        val groupResult = attrResult.flatMap { case (fields, ctx) =>
            attributeGroups.foldLeft[Either[String, (Map[ObjectLabel, Schema], TranslationContext)]](Right((fields, ctx))):
                case (Right((accFields, accCtx)), groupNode) =>
                    val refName = (groupNode \ "@ref").headOption.map(_.text)
                    refName match
                        case Some(ref) =>
                            // Use lookupAttributeGroup which checks both local and imported namespaces
                            accCtx.lookupAttributeGroup(ref) match
                                case Some(groupDef) =>
                                    // Recursively translate attributes from the group definition
                                    translateAttributes(groupDef, accCtx).map { case (groupFields, groupCtx) =>
                                        (accFields ++ groupFields, groupCtx)
                                    }
                                case None           =>
                                    val newCtx = accCtx.addLoss(
                                        s"XSD attributeGroup ref='$ref' not found",
                                        Some("Define the attribute group or import the schema that defines it")
                                    )
                                    Right((accFields, newCtx))
                        case None      =>
                            // Inline attributeGroup definition (shouldn't happen in well-formed XSD)
                            val newCtx = accCtx.addLoss(
                                "XSD attributeGroup without ref inside complexType",
                                Some("Use ref= to reference a named attributeGroup")
                            )
                            Right((accFields, newCtx))
                case (left @ Left(_), _)                     => left
        }

        // Handle xs:anyAttribute wildcards (just report the approximation, no fields added)
        groupResult.map { case (fields, ctx) =>
            val finalCtx = anyAttributes.foldLeft(ctx) { (accCtx, anyAttrNode) =>
                translateAnyAttribute(anyAttrNode, accCtx)
            }
            (fields, finalCtx)
        }

    /** Translates a single xs:attribute element */
    private def translateAttribute(attrNode: Node, context: TranslationContext): Either[String, (Option[String], Schema, Boolean, TranslationContext)] =
        val name       = (attrNode \ "@name").headOption.map(_.text)
        val typeName   = (attrNode \ "@type").headOption.map(_.text)
        val use        = (attrNode \ "@use").headOption.map(_.text).getOrElse("optional")
        val defaultVal = (attrNode \ "@default").headOption.map(_.text)
        val fixedVal   = (attrNode \ "@fixed").headOption.map(_.text)
        val ref        = (attrNode \ "@ref").headOption.map(_.text)
        val isRequired = use == "required"

        // Handle ref= attribute (reference to global attribute)
        if ref.isDefined then
            val refName = ref.get
            // Use lookupAttribute which checks both local and imported namespaces
            context.lookupAttribute(refName) match
                case Some(attrDef) =>
                    // Translate the referenced attribute definition
                    translateAttribute(attrDef, context)
                case None          =>
                    val ctx = context.addLoss(
                        s"XSD attribute ref='$refName' not found",
                        Some("Define the attribute or import the schema that defines it")
                    )
                    Right((None, TextValue(), false, ctx))
        else if name.isEmpty then
            val ctx = context.addLoss(
                "XSD attribute without name or ref",
                Some("Add a name attribute")
            )
            Right((None, TextValue(), false, ctx))
        else
            // Get the type
            val typeResult: Either[String, (Schema, TranslationContext)] = typeName match
                case Some(tn) => translateTypeReference(tn, context.atPath(s"@${ name.get }"))
                case None     =>
                    // Check for inline simpleType
                    val inlineTypes = (attrNode \ "simpleType").filter(_.prefix == context.xsPrefix)
                    inlineTypes.headOption match
                        case Some(inlineType) => translateSimpleType(inlineType, context.atPath(s"@${ name.get }"))
                        case None             => Right((TextValue(), context)) // Default to text

            typeResult.map { case (schema, ctx) =>
                // Handle fixed value as enum constraint
                val finalSchema = fixedVal match
                    case Some(fixed) => EnumValues(fixed)
                    case None        =>
                        defaultVal match
                            case Some(default) =>
                                // Add default value to schema
                                schema match
                                    case TextValue(constraints, _)    => TextValue(constraints, Some(default))
                                    case NumericValue(constraints, _) =>
                                        scala.util.Try(BigDecimal(default)).toOption match
                                            case Some(num) => NumericValue(constraints, Some(num))
                                            case None      => schema
                                    case BooleanValue(_)              =>
                                        default.toLowerCase match
                                            case "true" | "1"  => BooleanValue(Some(true))
                                            case "false" | "0" => BooleanValue(Some(false))
                                            case _             => schema
                                    case _                            => schema
                            case None          => schema

                (name, finalSchema, isRequired, ctx)
            }

    private def translateSequence(sequenceNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val elements   = (sequenceNode \ "element").filter(_.prefix == context.xsPrefix)
        val groupRefs  = (sequenceNode \ "group").filter(_.prefix == context.xsPrefix)
        val nestedSeqs = (sequenceNode \ "sequence").filter(_.prefix == context.xsPrefix)
        val choices    = (sequenceNode \ "choice").filter(_.prefix == context.xsPrefix)
        val anys       = (sequenceNode \ "any").filter(_.prefix == context.xsPrefix)

        // First translate individual elements
        val elemResults = elements.zipWithIndex.foldLeft[Either[String, (List[(String, Schema, Boolean)], TranslationContext)]](Right((List.empty, context))):
            case (Right((schemas, ctx)), (elem, idx)) =>
                translateElement(elem, ctx.atPath(s"element[$idx]")) match
                    case Left(error)             => Left(error)
                    case Right((schema, newCtx)) =>
                        // Get name from @name, or fall back to @ref for referenced elements
                        // For refs, resolve namespace prefix to get local name
                        val name       = (elem \ "@name").headOption
                            .map(_.text)
                            .filter(_.nonEmpty)
                            .orElse((elem \ "@ref").headOption.map(_.text).flatMap(ctx.resolveLocalName))
                            .getOrElse("")
                        val minOccurs  = (elem \ "@minOccurs").headOption.map(_.text).getOrElse("1")
                        val isOptional = minOccurs == "0"
                        Right((schemas :+ (name, schema, isOptional), newCtx))
            case (left @ Left(_), _)                  => left

        // Then expand group references
        val groupResults = elemResults.flatMap { case (elemList, ctx) =>
            groupRefs.foldLeft[Either[String, (List[(String, Schema, Boolean)], TranslationContext)]](Right((elemList, ctx))):
                case (Right((accList, accCtx)), groupNode) =>
                    val refName = (groupNode \ "@ref").headOption.map(_.text)
                    refName match
                        case Some(ref) =>
                            // Use lookupGroup which checks both local and imported namespaces
                            accCtx.lookupGroup(ref) match
                                case Some(groupDef) =>
                                    // Group should contain sequence, choice, or all
                                    translateGroupContent(groupDef, accCtx).map { case (groupElems, groupCtx) =>
                                        (accList ++ groupElems, groupCtx)
                                    }
                                case None           =>
                                    val newCtx = accCtx.addLoss(
                                        s"XSD group ref='$ref' not found",
                                        Some("Define the group or import the schema that defines it")
                                    )
                                    Right((accList, newCtx))
                        case None      =>
                            val newCtx = accCtx.addLoss(
                                "XSD group without ref inside sequence",
                                Some("Use ref= to reference a named group")
                            )
                            Right((accList, newCtx))
                case (left @ Left(_), _)                   => left
        }

        // Handle nested sequences
        val seqResults = groupResults.flatMap { case (accList, ctx) =>
            nestedSeqs.foldLeft[Either[String, (List[(String, Schema, Boolean)], TranslationContext)]](Right((accList, ctx))):
                case (Right((list, c)), seqNode) =>
                    translateSequence(seqNode, c).map:
                        case (ObjectValue(fields), newCtx) =>
                            val newElems = fields.map { case (label, schema) =>
                                val name       = label.label
                                val isOptional = label.isInstanceOf[OptionalLabel]
                                (name, schema, isOptional)
                            }.toList
                            (list ++ newElems, newCtx)
                        case (schema, newCtx)              =>
                            // Non-object result - treat as unnamed element
                            (list :+ ("", schema, false), newCtx)
                case (left @ Left(_), _)         => left
        }

        // Handle choices inside sequence (each choice becomes an element)
        val choiceResults = seqResults.flatMap { case (accList, ctx) =>
            choices.zipWithIndex.foldLeft[Either[String, (List[(String, Schema, Boolean)], TranslationContext)]](Right((accList, ctx))):
                case (Right((list, c)), (choiceNode, idx)) =>
                    translateChoice(choiceNode, c.atPath(s"choice[$idx]")).map { case (choiceSchema, newCtx) =>
                        // Choices in sequences are unnamed
                        (list :+ ("", choiceSchema, false), newCtx)
                    }
                case (left @ Left(_), _)                   => left
        }

        // Handle xs:any wildcards inside sequence
        val anyResults = choiceResults.flatMap { case (accList, ctx) =>
            anys.zipWithIndex.foldLeft[Either[String, (List[(String, Schema, Boolean)], TranslationContext)]](Right((accList, ctx))):
                case (Right((list, c)), (anyNode, idx)) =>
                    translateAny(anyNode, c.atPath(s"any[$idx]")).map { case (anySchema, newCtx) =>
                        // xs:any elements are unnamed wildcards
                        (list :+ ("", anySchema, false), newCtx)
                    }
                case (left @ Left(_), _)                => left
        }

        anyResults.map { case (allElements, ctx) =>
            if allElements.isEmpty then (ObjectValue(Map.empty), ctx)
            // If all elements are named, create an ObjectValue
            else if allElements.forall(_._1.nonEmpty) then
                val fields = allElements.map { case (name, schema, isOptional) =>
                    val label = if isOptional then OptionalLabel(name) else MandatoryLabel(name)
                    (label, schema)
                }.toMap
                (ObjectValue(fields), ctx)
            else
                // Otherwise create a TupleValue
                val schemas = allElements.map(_._2)
                (TupleValue(schemas*), ctx)
        }

    /** Translates the content of a xs:group definition (extracts sequence/choice/all content) */
    private def translateGroupContent(groupDef: Node, context: TranslationContext): Either[String, (List[(String, Schema, Boolean)], TranslationContext)] =
        val sequences = (groupDef \ "sequence").filter(_.prefix == context.xsPrefix)
        val choices   = (groupDef \ "choice").filter(_.prefix == context.xsPrefix)
        val alls      = (groupDef \ "all").filter(_.prefix == context.xsPrefix)

        if sequences.nonEmpty then
            translateSequence(sequences.head, context).map:
                case (ObjectValue(fields), ctx)      =>
                    val elems = fields.map { case (label, schema) =>
                        val name       = label.label
                        val isOptional = label.isInstanceOf[OptionalLabel]
                        (name, schema, isOptional)
                    }.toList
                    (elems, ctx)
                case (TupleValue(schemas @ _*), ctx) =>
                    val elems = schemas.map(s => ("", s, false)).toList
                    (elems, ctx)
                case (schema, ctx)                   =>
                    (List(("", schema, false)), ctx)
        else if choices.nonEmpty then
            translateChoice(choices.head, context).map { case (schema, ctx) =>
                (List(("", schema, false)), ctx)
            }
        else if alls.nonEmpty then
            val ctx = context.addApproximation(
                "XSD xs:all inside group converted to sequence",
                Some("Element order is not preserved")
            )
            translateSequence(alls.head, ctx).map:
                case (ObjectValue(fields), newCtx) =>
                    val elems = fields.map { case (label, schema) =>
                        val name       = label.label
                        val isOptional = label.isInstanceOf[OptionalLabel]
                        (name, schema, isOptional)
                    }.toList
                    (elems, newCtx)
                case (schema, newCtx)              =>
                    (List(("", schema, false)), newCtx)
        else
            // Empty group
            Right((List.empty, context))

    /** Translates a xs:group definition as a single Schema (for use in choices) */
    private def translateGroupAsSchema(groupDef: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val sequences = (groupDef \ "sequence").filter(_.prefix == context.xsPrefix)
        val choices   = (groupDef \ "choice").filter(_.prefix == context.xsPrefix)
        val alls      = (groupDef \ "all").filter(_.prefix == context.xsPrefix)

        if sequences.nonEmpty then translateSequence(sequences.head, context)
        else if choices.nonEmpty then translateChoice(choices.head, context)
        else if alls.nonEmpty then
            val ctx = context.addApproximation(
                "XSD xs:all inside group converted to sequence",
                Some("Element order is not preserved")
            )
            translateSequence(alls.head, ctx)
        else Right((ObjectValue(Map.empty), context))

    private def translateChoice(choiceNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val elements  = (choiceNode \ "element").filter(_.prefix == context.xsPrefix)
        val groupRefs = (choiceNode \ "group").filter(_.prefix == context.xsPrefix)
        val sequences = (choiceNode \ "sequence").filter(_.prefix == context.xsPrefix)
        val anys      = (choiceNode \ "any").filter(_.prefix == context.xsPrefix)

        // First translate individual elements
        val elemResults = elements.zipWithIndex.foldLeft[Either[String, (List[Schema], TranslationContext)]](Right((List.empty, context))):
            case (Right((schemas, ctx)), (elem, idx)) =>
                translateElement(elem, ctx.atPath(s"choice[$idx]")) match
                    case Left(error)             => Left(error)
                    case Right((schema, newCtx)) => Right((schemas :+ schema, newCtx))
            case (left @ Left(_), _)                  => left

        // Expand group references in choice - each group becomes a single alternative
        val groupResults = elemResults.flatMap { case (schemaList, ctx) =>
            groupRefs.foldLeft[Either[String, (List[Schema], TranslationContext)]](Right((schemaList, ctx))):
                case (Right((accList, accCtx)), groupNode) =>
                    val refName = (groupNode \ "@ref").headOption.map(_.text)
                    refName match
                        case Some(ref) =>
                            // Use lookupGroup which checks both local and imported namespaces
                            accCtx.lookupGroup(ref) match
                                case Some(groupDef) =>
                                    // In a choice, a group becomes a single alternative (translate as a whole schema)
                                    translateGroupAsSchema(groupDef, accCtx).map { case (groupSchema, groupCtx) =>
                                        (accList :+ groupSchema, groupCtx)
                                    }
                                case None           =>
                                    val newCtx = accCtx.addLoss(
                                        s"XSD group ref='$ref' not found",
                                        Some("Define the group or import the schema that defines it")
                                    )
                                    Right((accList, newCtx))
                        case None      =>
                            Right((accList, accCtx))
                case (left @ Left(_), _)                   => left
        }

        // Handle nested sequences in choice
        val seqResults = groupResults.flatMap { case (accList, ctx) =>
            sequences.zipWithIndex.foldLeft[Either[String, (List[Schema], TranslationContext)]](Right((accList, ctx))):
                case (Right((list, c)), (seqNode, idx)) =>
                    translateSequence(seqNode, c.atPath(s"sequence[$idx]")).map { case (seqSchema, newCtx) =>
                        (list :+ seqSchema, newCtx)
                    }
                case (left @ Left(_), _)                => left
        }

        // Handle xs:any wildcards in choice
        val anyResults = seqResults.flatMap { case (accList, ctx) =>
            anys.zipWithIndex.foldLeft[Either[String, (List[Schema], TranslationContext)]](Right((accList, ctx))):
                case (Right((list, c)), (anyNode, idx)) =>
                    translateAny(anyNode, c.atPath(s"any[$idx]")).map { case (anySchema, newCtx) =>
                        (list :+ anySchema, newCtx)
                    }
                case (left @ Left(_), _)                => left
        }

        anyResults.map { case (schemas, ctx) =>
            if schemas.isEmpty then (AnyValue(), ctx)
            else (AlternativeValues(schemas*), ctx)
        }

    /** Translates xs:any wildcard element */
    private def translateAny(anyNode: Node, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val namespace       = (anyNode \ "@namespace").headOption.map(_.text).getOrElse("##any")
        val processContents = (anyNode \ "@processContents").headOption.map(_.text).getOrElse("strict")
        val minOccurs       = (anyNode \ "@minOccurs").headOption.map(_.text).getOrElse("1")
        val maxOccurs       = (anyNode \ "@maxOccurs").headOption.map(_.text).getOrElse("1")

        // xs:any represents arbitrary XML elements - translate to MapValue(AnyValue())
        // This represents an object with any string keys mapping to any values
        val baseSchema = MapValue(AnyValue())

        val ctx = context.addApproximation(
            s"XSD xs:any (namespace='$namespace', processContents='$processContents') translated to MapValue(AnyValue())",
            Some("Namespace and processing constraints are not preserved")
        )

        // Handle occurrence constraints
        if maxOccurs == "unbounded" || maxOccurs.toIntOption.exists(_ > 1) then
            val sizeRange = (minOccurs.toIntOption, maxOccurs) match
                case (Some(min), "unbounded") if min > 0 =>
                    Some(ListConstraint.SizeRange.minInclusive(min))
                case (Some(min), max)                    =>
                    max.toIntOption match
                        case Some(maxInt) if min > 0 =>
                            Some(
                                ListConstraint.SizeRange(
                                    Some(BoundConstraint(BoundOp.MinInclusive, min)),
                                    Some(BoundConstraint(BoundOp.MaxInclusive, maxInt))
                                )
                            )
                        case Some(maxInt)            =>
                            Some(ListConstraint.SizeRange.maxInclusive(maxInt))
                        case None                    => None
                case _                                   => None
            Right((ListOfValues(baseSchema, ListConstraint.Constraints(size = sizeRange)), ctx))
        else if minOccurs == "0" then
            // Optional single xs:any - wrap in AlternativeValues with empty alternative
            Right((AlternativeValues(baseSchema, ObjectValue(Map.empty)), ctx))
        else Right((baseSchema, ctx))

    /** Translates xs:anyAttribute wildcard */
    private def translateAnyAttribute(anyAttrNode: Node, context: TranslationContext): TranslationContext =
        val namespace       = (anyAttrNode \ "@namespace").headOption.map(_.text).getOrElse("##any")
        val processContents = (anyAttrNode \ "@processContents").headOption.map(_.text).getOrElse("strict")

        context.addApproximation(
            s"XSD xs:anyAttribute (namespace='$namespace', processContents='$processContents') allows additional attributes",
            Some("Arbitrary attribute extensibility is not preserved in schema")
        )

package rengbis.translators.schemas.jsonschema

import rengbis.Schema.*
import rengbis.translators.common.*
import zio.json.*
import zio.json.ast.Json

/** Imports JSON Schema 2020-12 to ReNGBis schemas.
  *
  * This importer translates JSON Schema to ReNGBis schemas. Since JSON Schema has different semantics and capabilities, some features may not translate perfectly. The FrictionReport tracks any translation issues.
  */
object JsonSchemaImporter:

    /** Result of importing a JSON Schema, including the root schema and any named definitions. */
    case class ImportResult(
        root: Schema,
        definitions: Map[String, Schema],
        report: FrictionReport
    )

    /** Imports a JSON Schema to ReNGBis schema.
      *
      * @param jsonSchemaText
      *   The JSON Schema as a string
      * @return
      *   Either an error message or tuple of (ReNGBis Schema, FrictionReport)
      */
    def fromJsonSchema(jsonSchemaText: String): Either[String, (Schema, FrictionReport)] =
        fromJsonSchemaWithDefinitions(jsonSchemaText).map(r => (r.root, r.report))

    /** Imports a JSON Schema to ReNGBis schema, including named definitions.
      *
      * @param jsonSchemaText
      *   The JSON Schema as a string
      * @return
      *   Either an error message or ImportResult containing root schema, definitions, and friction report
      */
    def fromJsonSchemaWithDefinitions(jsonSchemaText: String): Either[String, ImportResult] =
        jsonSchemaText.fromJson[Json] match
            case Left(error)    => Left(s"Failed to parse JSON: $error")
            case Right(jsonAst) =>
                val context = TranslationContext(rootJson = jsonAst)
                translateSchema(jsonAst, context) match
                    case Left(error)          => Left(error)
                    case Right((schema, ctx)) =>
                        // Now translate all definitions that were referenced
                        val defsResult = translateAllDefinitions(jsonAst, ctx)
                        defsResult.map { case (definitions, finalCtx) =>
                            ImportResult(schema, definitions, finalCtx.report)
                        }

    private case class TranslationContext(
        path: String = "$",
        report: FrictionReport = FrictionReport(),
        definitions: Map[String, Json] = Map.empty,
        rootJson: Json = Json.Obj(),
        resolvedRefs: Set[String] = Set.empty, // Track refs being resolved to detect cycles
        referencedDefs: Set[String] = Set.empty // Track definitions that need to be output as named schemas
    ):
        def atPath(newPath: String): TranslationContext =
            copy(path = if newPath.startsWith("$") then newPath else s"$path/$newPath")

        def addLoss(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addLoss(path, message, suggestion))

        def addApproximation(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addApproximation(path, message, suggestion))

        def addExtension(message: String, suggestion: Option[String] = None): TranslationContext =
            copy(report = report.addExtension(path, message, suggestion))

        def withDefinitions(defs: Map[String, Json]): TranslationContext =
            copy(definitions = definitions ++ defs)

        def withResolvedRef(ref: String): TranslationContext =
            copy(resolvedRefs = resolvedRefs + ref)

        def isResolvingRef(ref: String): Boolean =
            resolvedRefs.contains(ref)

        def addReferencedDef(name: String): TranslationContext =
            copy(referencedDefs = referencedDefs + name)

    /** Resolves a JSON Pointer (RFC 6901) path within a JSON document.
      * @param json
      *   The root JSON document
      * @param pointer
      *   The JSON pointer path (e.g., "/properties/name" or "properties/name")
      * @return
      *   The JSON at the pointer location, or None if not found
      */
    private def resolveJsonPointer(json: Json, pointer: String): Option[Json] =
        val path = if pointer.startsWith("/") then pointer.substring(1) else pointer
        if path.isEmpty then Some(json)
        else
            val segments = path
                .split("/")
                .toList
                .map(s =>
                    // JSON Pointer escape sequences: ~1 = /, ~0 = ~
                    s.replace("~1", "/").replace("~0", "~")
                )
            segments.foldLeft[Option[Json]](Some(json)) { (current, segment) =>
                current.flatMap:
                    case Json.Obj(fields) => fields.toMap.get(segment)
                    case Json.Arr(items)  => segment.toIntOption.flatMap(i => items.lift(i))
                    case _                => None
            }

    /** Converts a JSON pointer path to a valid ReNGBis reference name. E.g., "/properties/ecmaFeatures" -> "properties_ecmaFeatures"
      */
    private def pointerToRefName(pointer: String): String =
        val path = if pointer.startsWith("/") then pointer.substring(1) else pointer
        path.replace("/", "_").replace("~1", "_").replace("~0", "_")

    private def translateSchema(json: Json, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        json match
            case Json.Bool(true)  => Right((AnyValue(), context)) // true schema = accept anything
            case Json.Bool(false) => Right((Fail(), context))     // false schema = reject everything
            case Json.Obj(fields) => translateObject(fields.toMap, context)
            case _                => Left(s"Invalid JSON Schema: expected object or boolean, got $json")

    private def translateObject(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        var currentCtx = context

        // Extract $defs first
        fields.get("$defs").orElse(fields.get("definitions")) match
            case Some(Json.Obj(defs)) => currentCtx = currentCtx.withDefinitions(defs.toMap)
            case _                    => ()

        // Handle $ref
        fields.get("$ref") match
            case Some(Json.Str(ref)) => translateRef(ref, currentCtx)
            case _                   =>
                // Handle type
                val typeResult = fields.get("type") match
                    case Some(Json.Str(typeName))  => translateTypedSchema(typeName, fields, currentCtx)
                    case Some(Json.Arr(typeArray)) => translateMultipleTypes(typeArray.toList, fields, currentCtx)
                    case None                      => translateUntyped(fields, currentCtx)
                    case Some(_)                   => Left("Invalid type field")

                typeResult.map { case (schema, ctx) =>
                    // Wrap with metadata if present
                    // Documentation can be applied to any schema type in ReNGBis when used as a property value
                    val withDoc = fields.get("description") match
                        case Some(Json.Str(desc)) => Documented(Some(desc), schema)
                        case _                    => schema

                    val withDeprecated = fields.get("deprecated") match
                        case Some(Json.Bool(true)) => Deprecated(withDoc)
                        case _                     => withDoc

                    (withDeprecated, ctx)
                }

    private def translateRef(ref: String, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        if ref.startsWith("#/$defs/") || ref.startsWith("#/definitions/") then
            // Internal reference to $defs/definitions - use named reference and track it
            val name       = ref.split("/").last
            // Track that this definition is referenced and needs to be output
            val updatedCtx = context.addReferencedDef(name)
            Right((NamedValueReference(name), updatedCtx))
        else if ref == "#" then
            // Self-reference (recursive schema)
            if context.isResolvingRef(ref) then
                // Already resolving this ref - it's a recursive reference
                // Use a placeholder name for the root schema (must start with letter)
                Right((NamedValueReference("root"), context))
            else
                // Resolve by translating the root schema
                val ctxWithRef = context.withResolvedRef(ref)
                translateSchema(context.rootJson, ctxWithRef)
        else if ref.startsWith("#/") then
            // Internal reference using JSON pointer (e.g., #/properties/ecmaFeatures)
            val pointer = ref.substring(1) // Remove the leading #
            if context.isResolvingRef(ref) then
                // Cycle detected - use a named reference
                val refName = pointerToRefName(pointer)
                Right((NamedValueReference(refName), context))
            else
                resolveJsonPointer(context.rootJson, pointer) match
                    case Some(referencedJson) =>
                        // Inline the referenced schema
                        val ctxWithRef = context.withResolvedRef(ref)
                        translateSchema(referencedJson, ctxWithRef)
                    case None                 =>
                        val currentCtx = context.addLoss(
                            s"Internal reference '$ref' could not be resolved",
                            Some("The referenced path does not exist in the schema")
                        )
                        Right((AnyValue(), currentCtx))
        else if ref.startsWith("#") then
            // Anchor reference (e.g., #myAnchor) - not supported
            val currentCtx = context.addApproximation(
                s"Anchor reference '$ref' not supported",
                Some("Use JSON pointer references like #/$$defs/name instead")
            )
            Right((AnyValue(), currentCtx))
        else if ref.contains("://") then
            // URL reference
            val currentCtx = context.addLoss(
                s"External URL reference '$ref' cannot be imported",
                Some("Download and inline the schema, or use file-based references")
            )
            Right((AnyValue(), currentCtx))
        else
            // File-based reference
            val parts = ref.split("#")
            val file  = parts(0)
            if parts.length > 1 then
                // Reference with fragment (e.g., "./person.json#/$defs/Address")
                val fragment  = parts(1)
                val defName   = fragment.split("/").last
                val namespace = file.replaceAll("[./]", "_").replaceAll("json$", "")
                Right((ScopedReference(namespace, defName), context))
            else
                // Reference to entire file (e.g., "./person.json")
                val namespace = file.replaceAll("[./]", "_").replaceAll("json$", "")
                Right((ScopedReference(namespace, ""), context))

    private def translateTypedSchema(typeName: String, fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        typeName match
            case "string"  => translateString(fields, context)
            case "number"  => translateNumber(fields, context, isInteger = false)
            case "integer" => translateNumber(fields, context, isInteger = true)
            case "boolean" =>
                val default = fields.get("default") match
                    case Some(Json.Bool(b)) => Some(b)
                    case _                  => None
                Right((BooleanValue(default), context))
            case "null"    =>
                val ctx = context.addApproximation(
                    "null type not directly supported in ReNGBis",
                    Some("Consider using optional fields or alternatives")
                )
                Right((AnyValue(), ctx))
            case "array"   => translateArray(fields, context)
            case "object"  => translateObjectType(fields, context)
            case _         =>
                val ctx = context.addLoss(s"Unknown type '$typeName'", None)
                Right((AnyValue(), ctx))

    private def translateMultipleTypes(types: List[Json], fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        var currentCtx = context

        // Filter out null types - in ReNGBis, nullability is handled through optional fields
        val nonNullTypes = types.collect { case Json.Str(t) if t != "null" => t }
        val hasNull      = types.exists { case Json.Str("null") => true; case _ => false }

        if hasNull && nonNullTypes.nonEmpty then
            currentCtx = currentCtx.addApproximation(
                "null in type array handled through field optionality",
                Some("In ReNGBis, use optional fields (name?) instead of nullable types")
            )

        // Pass the fields to each type translation so constraints like `items` are preserved
        val typeSchemas = nonNullTypes.flatMap: typeName =>
            translateTypedSchema(typeName, fields, currentCtx) match
                case Right((schema, ctx)) =>
                    currentCtx = ctx
                    Some(schema)
                case Left(_)              => None

        if typeSchemas.isEmpty then
            // All types were null or failed to translate
            if hasNull then Right((AnyValue(), currentCtx.addApproximation("type array containing only null", None)))
            else Left("No valid types in type array")
        else if typeSchemas.size == 1 then Right((typeSchemas.head, currentCtx))
        else
            // Only add approximation note if we actually have multiple non-null types
            currentCtx = currentCtx.addApproximation(
                s"Multiple types array translated to AlternativeValues (anyOf)",
                Some("Type arrays are converted to union types")
            )
            Right((AlternativeValues(typeSchemas*), currentCtx))

    private def translateUntyped(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        // Check for composition keywords first
        fields
            .get("anyOf")
            .map(translateAnyOf(_, context))
            .orElse(fields.get("oneOf").map(translateOneOf(_, context)))
            .orElse(fields.get("allOf").map(translateAllOf(_, context)))
            .orElse(fields.get("not").map { _ =>
                val ctx = context.addLoss(
                    "Negation (not) cannot be represented in ReNGBis",
                    Some("Consider restructuring schema to avoid negation")
                )
                Right((AnyValue(), ctx))
            })
            .orElse(fields.get("const").map {
                case Json.Str(value) => Right((GivenTextValue(value), context))
                case Json.Num(value) => Right((NumericValue(NumericConstraint.Constraints(value = Some(NumericConstraint.ValueRange.exact(BigDecimal(value)))), None), context))
                case _               =>
                    val ctx = context.addLoss("const with non-string/number value not supported", None)
                    Right((AnyValue(), ctx))
            })
            .orElse(fields.get("enum").map {
                case Json.Arr(values) =>
                    // Convert all enum values to strings
                    val allStringValues = values.toList.map {
                        case Json.Str(s)  => s
                        case Json.Num(n)  =>
                            val bd = BigDecimal(n)
                            if bd.isWhole then bd.toBigInt.toString else bd.toString
                        case Json.Bool(b) => b.toString
                        case Json.Null    => "null"
                        case other        => other.toString // fallback
                    }
                    val hasNonString    = values.exists {
                        case Json.Str(_) => false
                        case _           => true
                    }
                    if hasNonString then
                        val ctx = context.addApproximation(
                            "non-string enum values converted to strings",
                            Some("Numbers and booleans in enums are represented as string literals")
                        )
                        Right((EnumValues(allStringValues*), ctx))
                    else Right((EnumValues(allStringValues*), context))
                case _                => Right((AnyValue(), context))
            })
            // Handle implicit object schemas (have properties but no type)
            .orElse(fields.get("properties").map(_ => translateObjectType(fields, context)))
            // Handle implicit map schemas (have additionalProperties but no type or properties)
            .orElse(fields.get("additionalProperties").map(_ => translateObjectType(fields, context)))
            .getOrElse(Right((AnyValue(), context)))

    private def translateString(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        var sizeRange  = Option.empty[TextConstraint.SizeRange]
        var regex      = Option.empty[String]
        var format     = Option.empty[String]
        var currentCtx = context
        var default    = Option.empty[String]

        // minLength
        fields.get("minLength") match
            case Some(Json.Num(min)) =>
                val minRange = TextConstraint.SizeRange.minInclusive(BigDecimal(min).toInt)
                sizeRange = Some(sizeRange.map(_.merge(minRange)).getOrElse(minRange))
            case _                   => ()

        // maxLength
        fields.get("maxLength") match
            case Some(Json.Num(max)) =>
                val maxRange = TextConstraint.SizeRange.maxInclusive(BigDecimal(max).toInt)
                sizeRange = Some(sizeRange.map(_.merge(maxRange)).getOrElse(maxRange))
            case _                   => ()

        // pattern
        fields.get("pattern") match
            case Some(Json.Str(pattern)) =>
                regex = Some(pattern)
            case _                       => ()

        // format - check for special types first
        fields.get("format") match
            case Some(Json.Str(fmt)) =>
                fmt.toLowerCase match
                    case "date-time" | "date" | "time"                           =>
                        // This is actually a time value, not text
                        translateTimeFormat(fmt, currentCtx)
                    case "byte"                                                  =>
                        // base64-encoded binary
                        Right((BinaryValue(BinaryConstraint.Constraints(encoding = Some(BinaryConstraint.BinaryToTextEncoder.base64))), currentCtx))
                    case "email" | "uri" | "uuid" | "ipv4" | "ipv6" | "hostname" =>
                        format = Some(fmt.toLowerCase)
                        // default
                        fields.get("default") match
                            case Some(Json.Str(value)) => default = Some(value)
                            case _                     => ()
                        Right((TextValue(TextConstraint.Constraints(size = sizeRange, regex = regex, format = format), default), currentCtx))
                    case other                                                   =>
                        currentCtx = currentCtx.addExtension(
                            s"Custom format '$other' may not be supported",
                            Some("Standard formats: email, uri, uuid, ipv4, ipv6, hostname")
                        )
                        format = Some(other)
                        // default
                        fields.get("default") match
                            case Some(Json.Str(value)) => default = Some(value)
                            case _                     => ()
                        Right((TextValue(TextConstraint.Constraints(size = sizeRange, regex = regex, format = format), default), currentCtx))
            case _                   =>
                // default
                fields.get("default") match
                    case Some(Json.Str(value)) => default = Some(value)
                    case _                     => ()
                Right((TextValue(TextConstraint.Constraints(size = sizeRange, regex = regex, format = format), default), currentCtx))

    private def translateTimeFormat(format: String, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val timeFormat = format.toLowerCase match
            case "date-time" => TimeConstraint.NamedFormat.ISO8601_DateTime
            case "date"      => TimeConstraint.NamedFormat.ISO8601_Date
            case "time"      => TimeConstraint.NamedFormat.ISO8601_Time
            case _           => TimeConstraint.NamedFormat.ISO8601_DateTime

        Right((TimeValue(timeFormat), context))

    private def translateNumber(fields: Map[String, Json], context: TranslationContext, isInteger: Boolean): Either[String, (Schema, TranslationContext)] =
        var valueRange = Option.empty[NumericConstraint.ValueRange]
        var currentCtx = context
        var default    = Option.empty[BigDecimal]

        // minimum
        fields.get("minimum") match
            case Some(Json.Num(min)) =>
                val minRange = NumericConstraint.ValueRange.minInclusive(BigDecimal(min))
                valueRange = Some(valueRange.map(_.merge(minRange)).getOrElse(minRange))
            case _                   => ()

        // maximum
        fields.get("maximum") match
            case Some(Json.Num(max)) =>
                val maxRange = NumericConstraint.ValueRange.maxInclusive(BigDecimal(max))
                valueRange = Some(valueRange.map(_.merge(maxRange)).getOrElse(maxRange))
            case _                   => ()

        // exclusiveMinimum
        fields.get("exclusiveMinimum") match
            case Some(Json.Num(min)) =>
                val minRange = NumericConstraint.ValueRange.minExclusive(BigDecimal(min))
                valueRange = Some(valueRange.map(_.merge(minRange)).getOrElse(minRange))
            case _                   => ()

        // exclusiveMaximum
        fields.get("exclusiveMaximum") match
            case Some(Json.Num(max)) =>
                val maxRange = NumericConstraint.ValueRange.maxExclusive(BigDecimal(max))
                valueRange = Some(valueRange.map(_.merge(maxRange)).getOrElse(maxRange))
            case _                   => ()

        // multipleOf
        fields.get("multipleOf") match
            case Some(Json.Num(divisor)) =>
                currentCtx = currentCtx.addLoss(
                    s"multipleOf constraint ($divisor) cannot be represented in ReNGBis",
                    Some("Consider adding validation in application code")
                )
            case _                       => ()

        // default
        fields.get("default") match
            case Some(Json.Num(value)) => default = Some(BigDecimal(value))
            case _                     => ()

        Right((NumericValue(NumericConstraint.Constraints(value = valueRange, integer = isInteger), default), currentCtx))

    private def translateArray(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        var currentCtx = context

        // Check for tuple (prefixItems) - handle early
        val tupleResult = fields
            .get("prefixItems")
            .flatMap:
                case Json.Arr(items) =>
                    fields.get("items") match
                        case Some(Json.Bool(false)) | None => Some(translateTuple(items.toList, currentCtx))
                        case _                             => None
                case _               => None

        tupleResult.getOrElse:
            // Regular array with items
            fields.get("items") match
                case Some(itemSchema) =>
                    translateSchema(itemSchema, currentCtx.atPath("items")) match
                        case Left(error)          => Left(error)
                        case Right((schema, ctx)) =>
                            currentCtx = ctx
                            var sizeRange  = Option.empty[ListConstraint.SizeRange]
                            var uniqueness = Seq.empty[ListConstraint.Uniqueness]

                            // minItems
                            fields.get("minItems") match
                                case Some(Json.Num(min)) =>
                                    val minRange = ListConstraint.SizeRange.minInclusive(BigDecimal(min).toInt)
                                    sizeRange = Some(sizeRange.map(_.merge(minRange)).getOrElse(minRange))
                                case _                   => ()

                            // maxItems
                            fields.get("maxItems") match
                                case Some(Json.Num(max)) =>
                                    val maxRange = ListConstraint.SizeRange.maxInclusive(BigDecimal(max).toInt)
                                    sizeRange = Some(sizeRange.map(_.merge(maxRange)).getOrElse(maxRange))
                                case _                   => ()

                            // uniqueItems
                            fields.get("uniqueItems") match
                                case Some(Json.Bool(true)) =>
                                    uniqueness = Seq(ListConstraint.Uniqueness.Simple)
                                case _                     => ()

                            // contains/minContains/maxContains
                            if fields.contains("contains") || fields.contains("minContains") || fields.contains("maxContains") then
                                currentCtx = currentCtx.addLoss(
                                    "contains/minContains/maxContains constraints not supported in ReNGBis",
                                    Some("Use custom validation")
                                )

                            Right((ListOfValues(schema, ListConstraint.Constraints(size = sizeRange, unique = uniqueness)), currentCtx))
                case None             =>
                    // Array without items constraint - accepts any items
                    Right((ListOfValues(AnyValue()), currentCtx))

    private def translateTuple(items: List[Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        items.zipWithIndex
            .foldLeft[Either[String, (List[Schema], TranslationContext)]](Right((List.empty, context))):
                case (Right((schemas, ctx)), (item, idx)) =>
                    translateSchema(item, ctx.atPath(s"prefixItems[$idx]")) match
                        case Left(error)             => Left(error)
                        case Right((schema, newCtx)) => Right((schemas :+ schema, newCtx))
                case (left @ Left(_), _)                  => left
            .map { case (schemas, ctx) => (TupleValue(schemas*), ctx) }

    private def translateObjectType(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        var currentCtx = context

        // Check if this is a map (additionalProperties with no properties and no allOf)
        (fields.get("properties"), fields.get("additionalProperties"), fields.get("allOf")) match
            case (None, Some(additionalProps), None) if additionalProps != Json.Bool(false) =>
                // This is a map
                translateSchema(additionalProps, currentCtx.atPath("additionalProperties")).map { case (schema, ctx) =>
                    (MapValue(schema), ctx)
                }
            case _                                                                          =>
                // Check for allOf - this is common pattern for object schemas
                // where properties are defined in separate schemas and combined with allOf
                val allOfResult: Either[String, (Option[Schema], TranslationContext)] = fields.get("allOf") match
                    case Some(allOfJson) => translateAllOf(allOfJson, currentCtx).map { case (s, c) => (Some(s), c) }
                    case None            => Right((None, currentCtx))

                allOfResult match
                    case Left(error)                            => Left(error)
                    case Right((allOfSchemaOpt, ctxAfterAllOf)) =>
                        currentCtx = ctxAfterAllOf

                        // Also check for direct properties
                        val directPropsResult: Either[String, (Option[Schema], TranslationContext)] = fields.get("properties") match
                            case Some(Json.Obj(props)) if props.nonEmpty =>
                                val required = fields.get("required") match
                                    case Some(Json.Arr(reqs)) => reqs.toList.collect { case Json.Str(s) => s }.toSet
                                    case _                    => Set.empty[String]

                                props.toList
                                    .foldLeft[Either[String, (Map[ObjectLabel, Schema], TranslationContext)]](Right((Map.empty, currentCtx))):
                                        case (Right((objectFields, ctx)), (name, propSchema)) =>
                                            translateSchema(propSchema, ctx.atPath(s"properties/$name")) match
                                                case Left(error)             => Left(error)
                                                case Right((schema, newCtx)) =>
                                                    val label = if required.contains(name) then MandatoryLabel(name) else OptionalLabel(name)
                                                    Right((objectFields + (label -> schema), newCtx))
                                        case (left @ Left(_), _)                              => left
                                    .map { case (objectFields, ctx) => (Some(ObjectValue(objectFields)): Option[Schema], ctx) }
                            case _                                       => Right((None, currentCtx))

                        directPropsResult match
                            case Left(error)                            => Left(error)
                            case Right((directPropsOpt, ctxAfterProps)) =>
                                currentCtx = ctxAfterProps

                                // Merge allOf schema with direct properties
                                val mergedSchema: Schema = (allOfSchemaOpt, directPropsOpt) match
                                    case (Some(ov1: ObjectValue), Some(ov2: ObjectValue)) =>
                                        // Merge properties from both
                                        ObjectValue(ov1.obj ++ ov2.obj)
                                    case (Some(ov: ObjectValue), Some(_))                 => ov
                                    case (Some(_), Some(ov: ObjectValue))                 => ov
                                    case (Some(schema), Some(_))                          => schema
                                    case (Some(schema), None)                             => schema
                                    case (None, Some(schema))                             => schema
                                    case (None, None)                                     =>
                                        // Object without properties or allOf - treat as open map
                                        fields.get("additionalProperties") match
                                            case Some(Json.Bool(false)) =>
                                                currentCtx = currentCtx.addApproximation(
                                                    "closed empty object approximated as open map",
                                                    Some("JSON Schema with no properties and additionalProperties:false")
                                                )
                                            case _                      => ()
                                        MapValue(AnyValue())

                                var finalCtx = currentCtx

                                // Check for unsupported features
                                if fields.contains("patternProperties") then
                                    finalCtx = finalCtx.addLoss(
                                        "patternProperties not supported in ReNGBis",
                                        Some("Define explicit properties instead")
                                    )

                                if fields.contains("propertyNames") then
                                    finalCtx = finalCtx.addLoss(
                                        "propertyNames constraint not supported in ReNGBis",
                                        Some("Validation must be done externally")
                                    )

                                if fields.contains("minProperties") || fields.contains("maxProperties") then
                                    finalCtx = finalCtx.addLoss(
                                        "minProperties/maxProperties constraints not supported in ReNGBis",
                                        Some("Validation must be done externally")
                                    )

                                if fields.contains("dependentRequired") || fields.contains("dependentSchemas") then
                                    finalCtx = finalCtx.addLoss(
                                        "dependentRequired/dependentSchemas not supported in ReNGBis",
                                        Some("Document dependencies in description field")
                                    )

                                Right((mergedSchema, finalCtx))

    private def translateAnyOf(json: Json, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        json match
            case Json.Arr(schemas) =>
                schemas.toList.zipWithIndex
                    .foldLeft[Either[String, (List[Schema], TranslationContext)]](Right((List.empty, context))):
                        case (Right((translatedSchemas, ctx)), (schema, idx)) =>
                            translateSchema(schema, ctx.atPath(s"anyOf[$idx]")) match
                                case Left(error)                 => Left(error)
                                case Right((translated, newCtx)) => Right((translatedSchemas :+ translated, newCtx))
                        case (left @ Left(_), _)                              => left
                    .map { case (translatedSchemas, ctx) =>
                        // Unwrap single-element alternatives - they're semantically equivalent to the inner schema
                        val schema =
                            if translatedSchemas.size == 1 then translatedSchemas.head
                            else AlternativeValues(translatedSchemas*)
                        (schema, ctx)
                    }
            case _                 => Left("anyOf must be an array")

    private def translateOneOf(json: Json, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val ctx = context.addApproximation(
            "oneOf translated to AlternativeValues (anyOf) - exclusivity constraint lost",
            Some("ReNGBis AlternativeValues allows multiple matches, unlike oneOf")
        )
        translateAnyOf(json, ctx)

    private def translateAllOf(json: Json, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        json match
            case Json.Arr(schemas) =>
                val schemaList = schemas.toList
                if schemaList.isEmpty then Right((AnyValue(), context))
                else
                    // Translate all schemas in the allOf, inlining $ref definitions so we can merge properties
                    schemaList.zipWithIndex
                        .foldLeft[Either[String, (List[Schema], TranslationContext)]](Right((List.empty, context))):
                            case (Right((translatedSchemas, ctx)), (schema, idx)) =>
                                translateSchemaInliningRefs(schema, ctx.atPath(s"allOf[$idx]")) match
                                    case Left(error)                 => Left(error)
                                    case Right((translated, newCtx)) => Right((translatedSchemas :+ translated, newCtx))
                            case (left @ Left(_), _)                              => left
                        .map { case (translatedSchemas, ctx) =>
                            // Try to merge the schemas
                            mergeAllOfSchemas(translatedSchemas, ctx)
                        }

            case _ => Left("allOf must be an array")

    /** Translates a schema, inlining $ref definitions instead of creating NamedValueReference. This is used for allOf where we need to merge the actual properties.
      */
    private def translateSchemaInliningRefs(json: Json, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        json match
            case Json.Obj(fields) =>
                val fieldsMap = fields.toMap
                fieldsMap.get("$ref") match
                    case Some(Json.Str(ref)) if ref.startsWith("#/$defs/") || ref.startsWith("#/definitions/") =>
                        // Inline the definition instead of creating a NamedValueReference
                        val name    = ref.split("/").last
                        val pointer = if ref.startsWith("#/$defs/") then "/$defs/" + name else "/definitions/" + name

                        // Still track the reference for output
                        val ctxWithRef = context.addReferencedDef(name)

                        if ctxWithRef.isResolvingRef(ref) then
                            // Cycle detected - use named reference to break the cycle
                            Right((NamedValueReference(name), ctxWithRef))
                        else
                            resolveJsonPointer(ctxWithRef.rootJson, pointer) match
                                case Some(referencedJson) =>
                                    val ctxResolving = ctxWithRef.withResolvedRef(ref)
                                    translateSchema(referencedJson, ctxResolving)
                                case None                 =>
                                    // Definition not found - use named reference
                                    Right((NamedValueReference(name), ctxWithRef))
                    case _                                                                                     =>
                        // Not a definition $ref, use normal translation
                        translateSchema(json, context)
            case _                => translateSchema(json, context)

    /** Merges multiple schemas from an allOf into a single schema. For object schemas, properties are merged. For other types, only the first schema is used.
      */
    private def mergeAllOfSchemas(schemas: List[Schema], context: TranslationContext): (Schema, TranslationContext) =
        if schemas.isEmpty then (AnyValue(), context)
        else if schemas.size == 1 then (schemas.head, context)
        else
            // Collect all ObjectValue schemas (including Documented ones) and their properties
            val (objectSchemas, nonObjectSchemas) = schemas.partition:
                case _: ObjectValue                => true
                case Documented(_, _: ObjectValue) => true
                case _                             => false

            val objectValues = objectSchemas.flatMap:
                case ov: ObjectValue                => Some(ov)
                case Documented(_, ov: ObjectValue) => Some(ov)
                case _                              => None

            if objectValues.nonEmpty then
                // Merge all object properties
                val mergedProperties = objectValues.flatMap(_.obj).toMap
                var currentCtx       = context

                // If there are non-object schemas, report friction
                if nonObjectSchemas.nonEmpty then
                    currentCtx = currentCtx.addApproximation(
                        s"allOf contains ${ nonObjectSchemas.size } non-object schema(s) that were merged with object properties",
                        Some("Non-object schemas in allOf with object schemas may have different semantics")
                    )

                (ObjectValue(mergedProperties), currentCtx)
            else
                // No object schemas - report friction and use first schema
                val currentCtx = context.addLoss(
                    "allOf intersection semantics cannot be fully preserved in ReNGBis",
                    Some("Consider merging constraints manually or using composition")
                )
                (schemas.head, currentCtx)

    /** Translates all referenced definitions from the JSON Schema to ReNGBis schemas. This recursively translates definitions, discovering new referenced definitions as it goes.
      */
    private def translateAllDefinitions(rootJson: Json, context: TranslationContext): Either[String, (Map[String, Schema], TranslationContext)] =
        // Get the definitions object from the root JSON
        val defsJson: Map[String, Json] = rootJson match
            case Json.Obj(fields) =>
                val fieldsMap = fields.toMap
                fieldsMap.get("$defs").orElse(fieldsMap.get("definitions")) match
                    case Some(Json.Obj(defs)) => defs.toMap
                    case _                    => Map.empty
            case _                => Map.empty

        if defsJson.isEmpty then Right((Map.empty, context))
        else
            // Iteratively translate definitions until no new ones are discovered
            var currentCtx        = context
            var translatedDefs    = Map.empty[String, Schema]
            var pendingDefs       = currentCtx.referencedDefs
            var processedDefNames = Set.empty[String]

            while pendingDefs.nonEmpty do
                val defName = pendingDefs.head
                pendingDefs = pendingDefs - defName

                if !processedDefNames.contains(defName) then
                    processedDefNames = processedDefNames + defName

                    defsJson.get(defName) match
                        case Some(defJson) =>
                            // Create a fresh context for translating this definition to avoid interference
                            val defContext = TranslationContext(
                                path = s"$$/$defName",
                                report = currentCtx.report,
                                definitions = defsJson,
                                rootJson = rootJson,
                                resolvedRefs = Set.empty,
                                referencedDefs = currentCtx.referencedDefs
                            )

                            translateSchema(defJson, defContext) match
                                case Left(error)          =>
                                    // Add error to report but continue with other definitions
                                    currentCtx = currentCtx.addLoss(
                                        s"Failed to translate definition '$defName': $error",
                                        None
                                    )
                                case Right((schema, ctx)) =>
                                    translatedDefs = translatedDefs + (defName -> schema)
                                    // Merge newly discovered references
                                    val newRefs = ctx.referencedDefs -- processedDefNames
                                    pendingDefs = pendingDefs ++ newRefs
                                    currentCtx = currentCtx.copy(
                                        report = ctx.report,
                                        referencedDefs = currentCtx.referencedDefs ++ ctx.referencedDefs
                                    )
                        case None          =>
                            currentCtx = currentCtx.addLoss(
                                s"Referenced definition '$defName' not found in schema",
                                Some("Check that the definition exists in $defs or definitions")
                            )

            Right((translatedDefs, currentCtx))

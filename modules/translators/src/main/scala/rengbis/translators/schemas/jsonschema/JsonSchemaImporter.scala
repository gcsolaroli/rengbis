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

    /** Imports a JSON Schema to ReNGBis schema.
      *
      * @param jsonSchemaText
      *   The JSON Schema as a string
      * @return
      *   Either an error message or tuple of (ReNGBis Schema, FrictionReport)
      */
    def fromJsonSchema(jsonSchemaText: String): Either[String, (Schema, FrictionReport)] =
        jsonSchemaText.fromJson[Json] match
            case Left(error)    => Left(s"Failed to parse JSON: $error")
            case Right(jsonAst) =>
                val context = TranslationContext()
                translateSchema(jsonAst, context) match
                    case Left(error)          => Left(error)
                    case Right((schema, ctx)) => Right((schema, ctx.report))

    private case class TranslationContext(
        path: String = "$",
        report: FrictionReport = FrictionReport(),
        definitions: Map[String, Json] = Map.empty
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
            // Internal reference
            val name = ref.split("/").last
            Right((NamedValueReference(name), context))
        else if ref.startsWith("#") then
            // Other internal reference (anchor, etc.)
            val currentCtx = context.addApproximation(
                s"Internal reference '$ref' using anchors not fully supported",
                Some("Only #/$$defs/ references are fully supported")
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
            case "boolean" => Right((BooleanValue(), context))
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
        var currentCtx = context.addApproximation(
            s"Multiple types array translated to AlternativeValues (anyOf)",
            Some("Type arrays are converted to union types")
        )

        val typeSchemas = types.flatMap:
            case Json.Str(typeName) =>
                translateTypedSchema(typeName, Map.empty, currentCtx) match
                    case Right((schema, ctx)) =>
                        currentCtx = ctx
                        Some(schema)
                    case Left(_)              => None
            case _                  => None

        if typeSchemas.isEmpty then Left("No valid types in type array")
        else Right((AlternativeValues(typeSchemas*), currentCtx))

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
                case Json.Num(value) => Right((NumericValue(Seq(NumericConstraint.Value(BoundConstraint(BoundOp.Exact, value))), None), context))
                case _               =>
                    val ctx = context.addLoss("const with non-string/number value not supported", None)
                    Right((AnyValue(), ctx))
            })
            .orElse(fields.get("enum").map {
                case Json.Arr(values) =>
                    val stringValues = values.toList.collect { case Json.Str(s) => s }
                    if stringValues.size == values.size then Right((EnumValues(stringValues*), context))
                    else
                        val ctx = context.addLoss("enum with non-string values not fully supported", Some("Only string enums are supported"))
                        Right((EnumValues(stringValues*), ctx))
                case _                => Right((AnyValue(), context))
            })
            .getOrElse(Right((AnyValue(), context)))

    private def translateString(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        var constraints = Seq.empty[TextConstraint.Constraint]
        var currentCtx  = context
        var default     = Option.empty[String]

        // minLength
        fields.get("minLength") match
            case Some(Json.Num(min)) =>
                constraints = constraints :+ TextConstraint.Size(BoundConstraint(BoundOp.MinInclusive, BigDecimal(min).toInt))
            case _                   => ()

        // maxLength
        fields.get("maxLength") match
            case Some(Json.Num(max)) =>
                constraints = constraints :+ TextConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, BigDecimal(max).toInt))
            case _                   => ()

        // pattern
        fields.get("pattern") match
            case Some(Json.Str(pattern)) =>
                constraints = constraints :+ TextConstraint.Regex(pattern)
            case _                       => ()

        // format - check for special types first
        fields.get("format") match
            case Some(Json.Str(format)) =>
                format.toLowerCase match
                    case "date-time" | "date" | "time"                           =>
                        // This is actually a time value, not text
                        translateTimeFormat(format, currentCtx)
                    case "byte"                                                  =>
                        // base64-encoded binary
                        Right((BinaryValue(BinaryConstraint.Encoding(BinaryConstraint.BinaryToTextEncoder.base64)), currentCtx))
                    case "email" | "uri" | "uuid" | "ipv4" | "ipv6" | "hostname" =>
                        constraints = constraints :+ TextConstraint.Format(format.toLowerCase)
                        // default
                        fields.get("default") match
                            case Some(Json.Str(value)) => default = Some(value)
                            case _                     => ()
                        Right((TextValue(constraints, default), currentCtx))
                    case other                                                   =>
                        currentCtx = currentCtx.addExtension(
                            s"Custom format '$other' may not be supported",
                            Some("Standard formats: email, uri, uuid, ipv4, ipv6, hostname")
                        )
                        constraints = constraints :+ TextConstraint.Format(other)
                        // default
                        fields.get("default") match
                            case Some(Json.Str(value)) => default = Some(value)
                            case _                     => ()
                        Right((TextValue(constraints, default), currentCtx))
            case _                      =>
                // default
                fields.get("default") match
                    case Some(Json.Str(value)) => default = Some(value)
                    case _                     => ()
                Right((TextValue(constraints, default), currentCtx))

    private def translateTimeFormat(format: String, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val timeFormat = format.toLowerCase match
            case "date-time" => TimeConstraint.NamedFormat.ISO8601_DateTime
            case "date"      => TimeConstraint.NamedFormat.ISO8601_Date
            case "time"      => TimeConstraint.NamedFormat.ISO8601_Time
            case _           => TimeConstraint.NamedFormat.ISO8601_DateTime

        Right((TimeValue(timeFormat), context))

    private def translateNumber(fields: Map[String, Json], context: TranslationContext, isInteger: Boolean): Either[String, (Schema, TranslationContext)] =
        var constraints = Seq.empty[NumericConstraint.Constraint]
        var currentCtx  = context
        var default     = Option.empty[BigDecimal]

        if isInteger then constraints = constraints :+ NumericConstraint.Integer

        // minimum
        fields.get("minimum") match
            case Some(Json.Num(min)) =>
                constraints = constraints :+ NumericConstraint.Value(BoundConstraint(BoundOp.MinInclusive, min))
            case _                   => ()

        // maximum
        fields.get("maximum") match
            case Some(Json.Num(max)) =>
                constraints = constraints :+ NumericConstraint.Value(BoundConstraint(BoundOp.MaxInclusive, max))
            case _                   => ()

        // exclusiveMinimum
        fields.get("exclusiveMinimum") match
            case Some(Json.Num(min)) =>
                constraints = constraints :+ NumericConstraint.Value(BoundConstraint(BoundOp.MinExclusive, min))
            case _                   => ()

        // exclusiveMaximum
        fields.get("exclusiveMaximum") match
            case Some(Json.Num(max)) =>
                constraints = constraints :+ NumericConstraint.Value(BoundConstraint(BoundOp.MaxExclusive, max))
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
            case Some(Json.Num(value)) => default = Some(value)
            case _                     => ()

        Right((NumericValue(constraints, default), currentCtx))

    private def translateArray(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        var currentCtx = context

        // Check for tuple (prefixItems) - handle early
        val tupleResult = fields.get("prefixItems").flatMap:
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
                            var constraints = Seq.empty[ListConstraint.Constraint]

                            // minItems
                            fields.get("minItems") match
                                case Some(Json.Num(min)) =>
                                    constraints = constraints :+ ListConstraint.Size(BoundConstraint(BoundOp.MinInclusive, BigDecimal(min).toInt))
                                case _                   => ()

                            // maxItems
                            fields.get("maxItems") match
                                case Some(Json.Num(max)) =>
                                    constraints = constraints :+ ListConstraint.Size(BoundConstraint(BoundOp.MaxInclusive, BigDecimal(max).toInt))
                                case _                   => ()

                            // uniqueItems
                            fields.get("uniqueItems") match
                                case Some(Json.Bool(true)) =>
                                    constraints = constraints :+ ListConstraint.Unique
                                case _                     => ()

                            // contains/minContains/maxContains
                            if fields.contains("contains") || fields.contains("minContains") || fields.contains("maxContains") then
                                currentCtx = currentCtx.addLoss(
                                    "contains/minContains/maxContains constraints not supported in ReNGBis",
                                    Some("Use custom validation")
                                )

                            Right((ListOfValues(schema, constraints*), currentCtx))
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

        // Check if this is a map (additionalProperties with no properties)
        (fields.get("properties"), fields.get("additionalProperties")) match
            case (None, Some(additionalProps)) if additionalProps != Json.Bool(false) =>
                // This is a map
                translateSchema(additionalProps, currentCtx.atPath("additionalProperties")).map { case (schema, ctx) =>
                    (MapValue(schema), ctx)
                }
            case _                                                                    =>
                // Regular object with properties
                fields.get("properties") match
                    case Some(Json.Obj(props)) =>
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
                            .map { case (objectFields, ctx) =>
                                var finalCtx = ctx

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

                                (ObjectValue(objectFields), finalCtx)
                            }

                    case None =>
                        // Object without properties - empty object or map
                        fields.get("additionalProperties") match
                            case Some(Json.Bool(false)) => Right((ObjectValue(Map.empty), currentCtx))
                            case _                      => Right((MapValue(AnyValue()), currentCtx))

                    case _ => Left("Invalid properties definition")

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
                    .map { case (translatedSchemas, ctx) => (AlternativeValues(translatedSchemas*), ctx) }
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
                // Try to merge schemas
                var currentCtx = context
                val schemaList = schemas.toList

                // For now, report as friction and use first schema
                // TODO: Implement smart merging for compatible types
                currentCtx = currentCtx.addLoss(
                    "allOf intersection semantics cannot be fully preserved in ReNGBis",
                    Some("Consider merging constraints manually or using composition")
                )

                if schemaList.nonEmpty then translateSchema(schemaList.head, currentCtx.atPath("allOf[0]"))
                else Right((AnyValue(), currentCtx))

            case _ => Left("allOf must be an array")

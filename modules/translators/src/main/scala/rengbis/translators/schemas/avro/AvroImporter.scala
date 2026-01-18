package rengbis.translators.schemas.avro

import rengbis.Schema.*
import rengbis.translators.common.{ FrictionReport, FrictionType }
import zio.json.*
import zio.json.ast.Json

object AvroImporter:

    case class TranslationContext(
        path: String,
        report: FrictionReport,
        namedTypes: Map[String, Schema] = Map.empty
    ):
        def withPath(newPath: String): TranslationContext =
            copy(path = newPath)

        def addFriction(frictionType: FrictionType, message: String): TranslationContext =
            val updatedReport = frictionType match
                case FrictionType.Loss          => report.addLoss(path, message)
                case FrictionType.Approximation => report.addApproximation(path, message)
                case FrictionType.Extension     => report.addExtension(path, message)
            copy(report = updatedReport)

        def addNamedType(name: String, schema: Schema): TranslationContext =
            copy(namedTypes = namedTypes + (name -> schema))

    def fromAvro(avroJson: String): Either[String, (Schema, FrictionReport)] =
        avroJson.fromJson[Json] match
            case Left(error) => Left(s"Failed to parse Avro schema JSON: $error")
            case Right(json) =>
                val context = TranslationContext("", FrictionReport())
                translateType(json, context) match
                    case Left(error)                   => Left(error)
                    case Right((schema, finalContext)) => Right((schema, finalContext.report))

    private def translateType(json: Json, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        json match
            // Primitive types as strings
            case Json.Str("null") =>
                val updatedContext = context.addFriction(
                    FrictionType.Approximation,
                    "Avro 'null' type approximated as optional field (context-dependent)"
                )
                Right((AnyValue(), updatedContext)) // Placeholder, should be handled in union context

            case Json.Str("boolean") =>
                Right((BooleanValue(), context))

            case Json.Str("int") =>
                Right((NumericValue(NumericConstraint.Constraints(integer = true)), context))

            case Json.Str("long") =>
                Right((NumericValue(NumericConstraint.Constraints(integer = true)), context))

            case Json.Str("float") =>
                Right((NumericValue(), context))

            case Json.Str("double") =>
                Right((NumericValue(), context))

            case Json.Str("bytes") =>
                Right((BinaryValue(), context))

            case Json.Str("string") =>
                Right((TextValue(), context))

            // Named type reference
            case Json.Str(typeName) if context.namedTypes.contains(typeName) =>
                Right((NamedValueReference(typeName), context))

            case Json.Str(typeName) =>
                Left(s"Unknown type reference: $typeName")

            // Complex types as objects
            case Json.Obj(fieldsChunk) =>
                val fields = fieldsChunk.toMap
                fields.get("type") match
                    case Some(Json.Str("record")) =>
                        translateRecord(fields, context)

                    case Some(Json.Str("enum")) =>
                        translateEnum(fields, context)

                    case Some(Json.Str("array")) =>
                        translateArray(fields, context)

                    case Some(Json.Str("map")) =>
                        translateMap(fields, context)

                    case Some(Json.Str("fixed")) =>
                        translateFixed(fields, context)

                    case Some(Json.Str(primitiveType)) =>
                        // Primitive with logical type
                        fields.get("logicalType") match
                            case Some(Json.Str(logicalType)) =>
                                translateLogicalType(primitiveType, logicalType, context)
                            case _                           =>
                                translateType(Json.Str(primitiveType), context)

                    case Some(other) =>
                        Left(s"Invalid type specification: $other")

                    case None =>
                        Left("Missing 'type' field in Avro schema object")

            // Union types as arrays
            case Json.Arr(elements) =>
                translateUnion(elements.toList, context)

            case other =>
                Left(s"Unexpected JSON type in Avro schema: $other")

    private def translateRecord(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val name = fields
            .get("name")
            .flatMap:
                case Json.Str(n) => Some(n)
                case _           => None
            .getOrElse("Record")

        val doc = fields
            .get("doc")
            .flatMap:
                case Json.Str(d) => Some(d)
                case _           => None

        fields.get("fields") match
            case Some(Json.Arr(fieldDefs)) =>
                var currentContext = context
                val rengbisFields  = fieldDefs.flatMap { fieldDef =>
                    translateRecordField(fieldDef, currentContext) match
                        case Left(error)                          =>
                            currentContext = currentContext.addFriction(
                                FrictionType.Loss,
                                s"Failed to translate field: $error"
                            )
                            None
                        case Right(((label, schema), newContext)) =>
                            currentContext = newContext
                            Some((label, schema))
                }

                val objectSchema = ObjectValue(rengbisFields.toMap)
                val finalSchema  = doc match
                    case Some(d) => Documented(Some(d), objectSchema)
                    case None    => objectSchema

                Right((finalSchema, currentContext))

            case Some(other) =>
                Left(s"Invalid 'fields' value in record: $other")

            case None =>
                Left("Missing 'fields' in record type")

    private def translateRecordField(fieldJson: Json, context: TranslationContext): Either[String, ((ObjectLabel, Schema), TranslationContext)] =
        fieldJson match
            case Json.Obj(fieldsChunk) =>
                val fields = fieldsChunk.toMap
                fields
                    .get("name")
                    .flatMap:
                        case Json.Str(n) => Some(n)
                        case _           => None
                match
                    case None            => Left("Missing field name")
                    case Some(fieldName) =>
                        val fieldPath = if context.path.isEmpty then fieldName else s"${ context.path }.$fieldName"

                        fields.get("type") match
                            case Some(fieldType) =>
                                // Check if this is an optional field (union with null)
                                fieldType match
                                    case Json.Arr(elements) if isNullableUnion(elements.toList) =>
                                        // Extract non-null type
                                        val nonNullType = elements.find(_ != Json.Str("null")).get
                                        translateType(nonNullType, context.withPath(fieldPath)) match
                                            case Left(error)                        => Left(error)
                                            case Right((innerSchema, innerContext)) =>
                                                Right(((OptionalLabel(fieldName), innerSchema), innerContext))
                                    case _                                                      =>
                                        translateType(fieldType, context.withPath(fieldPath)) match
                                            case Left(error)                 => Left(error)
                                            case Right((schema, newContext)) =>
                                                Right(((MandatoryLabel(fieldName), schema), newContext))

                            case None =>
                                Left(s"Missing type for field: $fieldName")

            case other =>
                Left(s"Invalid field definition: $other")

    private def translateEnum(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        fields.get("symbols") match
            case Some(Json.Arr(symbols)) =>
                val symbolStrings = symbols.flatMap:
                    case Json.Str(s) => Some(s)
                    case _           => None
                if symbolStrings.size != symbols.size then Left("Invalid symbol in enum definition")
                else Right((EnumValues(symbolStrings*), context))

            case Some(other) =>
                Left(s"Invalid 'symbols' value in enum: $other")

            case None =>
                Left("Missing 'symbols' in enum type")

    private def translateArray(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        fields.get("items") match
            case Some(itemType) =>
                translateType(itemType, context.withPath(s"${ context.path }[]")) match
                    case Left(error)                     => Left(error)
                    case Right((itemSchema, newContext)) =>
                        Right((ListOfValues(itemSchema), newContext))

            case None =>
                Left("Missing 'items' in array type")

    private def translateMap(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        fields.get("values") match
            case Some(valueType) =>
                translateType(valueType, context.withPath(s"${ context.path }{}")) match
                    case Left(error)                      => Left(error)
                    case Right((valueSchema, newContext)) =>
                        Right((MapValue(valueSchema), newContext))

            case None =>
                Left("Missing 'values' in map type")

    private def translateFixed(fields: Map[String, Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        val size = fields
            .get("size")
            .flatMap:
                case Json.Num(n) => Some(n.intValue())
                case _           => None

        val updatedContext = size match
            case Some(s) =>
                context.addFriction(
                    FrictionType.Loss,
                    s"Fixed-size binary type (size=$s) translated to variable-length binary"
                )
            case None    =>
                context.addFriction(
                    FrictionType.Loss,
                    "Fixed-size binary type translated to variable-length binary"
                )

        Right((BinaryValue(), updatedContext))

    private def translateUnion(types: List[Json], context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        if isNullableUnion(types) then
            // Special case: [null, T] -> optional T
            val nonNullType = types.find(_ != Json.Str("null")).get
            translateType(nonNullType, context)
        else
            // General union -> AlternativeValues
            var currentContext = context
            val alternatives   = types.map { typeJson =>
                translateType(typeJson, currentContext) match
                    case Left(error)                 =>
                        currentContext = currentContext.addFriction(
                            FrictionType.Loss,
                            s"Failed to translate union member: $error"
                        )
                        None
                    case Right((schema, newContext)) =>
                        currentContext = newContext
                        Some(schema)
            }.flatten

            if alternatives.isEmpty then Left("Empty union or all members failed to translate")
            else if alternatives.size == 1 then Right((alternatives.head, currentContext))
            else Right((AlternativeValues(alternatives*), currentContext))

    private def translateLogicalType(primitiveType: String, logicalType: String, context: TranslationContext): Either[String, (Schema, TranslationContext)] =
        (primitiveType, logicalType) match
            case ("long", "timestamp-millis") =>
                Right((TimeValue(TimeConstraint.NamedFormat.ISO8601), context))

            case ("long", "timestamp-micros") =>
                val updatedContext = context.addFriction(
                    FrictionType.Approximation,
                    "Avro timestamp-micros approximated as ISO8601 timestamp"
                )
                Right((TimeValue(TimeConstraint.NamedFormat.ISO8601), updatedContext))

            case ("long", "local-timestamp-millis") =>
                val updatedContext = context.addFriction(
                    FrictionType.Approximation,
                    "Avro local-timestamp-millis approximated as ISO8601 timestamp"
                )
                Right((TimeValue(TimeConstraint.NamedFormat.ISO8601), updatedContext))

            case ("long", "local-timestamp-micros") =>
                val updatedContext = context.addFriction(
                    FrictionType.Approximation,
                    "Avro local-timestamp-micros approximated as ISO8601 timestamp"
                )
                Right((TimeValue(TimeConstraint.NamedFormat.ISO8601), updatedContext))

            case ("int", "date") =>
                Right((TimeValue(TimeConstraint.NamedFormat.ISO8601_Date), context))

            case ("int", "time-millis") =>
                Right((TimeValue(TimeConstraint.NamedFormat.ISO8601_Time), context))

            case ("long", "time-micros") =>
                val updatedContext = context.addFriction(
                    FrictionType.Approximation,
                    "Avro time-micros approximated as ISO8601 time"
                )
                Right((TimeValue(TimeConstraint.NamedFormat.ISO8601_Time), updatedContext))

            case ("bytes" | "fixed", "decimal") =>
                val updatedContext = context.addFriction(
                    FrictionType.Approximation,
                    "Avro decimal logical type approximated as ReNGBis number"
                )
                Right((NumericValue(), updatedContext))

            case ("string", "uuid") =>
                val updatedContext = context.addFriction(
                    FrictionType.Loss,
                    "Avro UUID logical type translated to string (UUID format constraint lost)"
                )
                Right((TextValue(), updatedContext))

            case (_, "duration") =>
                val updatedContext = context.addFriction(
                    FrictionType.Loss,
                    "Avro duration logical type not supported, using string"
                )
                Right((TextValue(), updatedContext))

            case _ =>
                val updatedContext = context.addFriction(
                    FrictionType.Loss,
                    s"Unknown logical type: $logicalType on $primitiveType, using base type"
                )
                translateType(Json.Str(primitiveType), updatedContext)

    private def isNullableUnion(types: List[Json]): Boolean =
        types.size == 2 && types.contains(Json.Str("null"))

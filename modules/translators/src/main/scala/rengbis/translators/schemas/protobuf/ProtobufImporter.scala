package rengbis.translators.schemas.protobuf

import rengbis.Schema.*
import rengbis.translators.common.{ FrictionReport, FrictionType }
import scala.util.matching.Regex

object ProtobufImporter:

    case class TranslationContext(
        path: String,
        report: FrictionReport,
        messages: Map[String, Schema] = Map.empty,
        enums: Map[String, Schema] = Map.empty,
        packageName: Option[String] = None
    ):
        def withPath(newPath: String): TranslationContext =
            copy(path = newPath)

        def addFriction(frictionType: FrictionType, message: String): TranslationContext =
            val updatedReport = frictionType match
                case FrictionType.Loss          => report.addLoss(path, message)
                case FrictionType.Approximation => report.addApproximation(path, message)
                case FrictionType.Extension     => report.addExtension(path, message)
            copy(report = updatedReport)

        def addMessage(name: String, schema: Schema): TranslationContext =
            copy(messages = messages + (name -> schema))

        def addEnum(name: String, schema: Schema): TranslationContext =
            copy(enums = enums + (name -> schema))

        def lookupType(name: String): Option[Schema] =
            messages.get(name).orElse(enums.get(name))

    case class ImportResult(
        definitions: Map[String, Schema],
        report: FrictionReport
    )

    /** Imports a Protocol Buffers proto3 file to ReNGBis schema definitions. */
    def fromProtobuf(protoContent: String): Either[String, ImportResult] =
        val context = TranslationContext("", FrictionReport())

        // Remove comments
        val cleaned = removeComments(protoContent)

        // Check syntax
        if !cleaned.contains("syntax") then return Left("Missing syntax declaration")

        val syntaxMatch = """syntax\s*=\s*"proto3"\s*;""".r.findFirstIn(cleaned)
        if syntaxMatch.isEmpty then
            val ctx = context.addFriction(
                FrictionType.Approximation,
                "Non-proto3 syntax detected, some features may not translate correctly"
            )

        // Extract package name
        val packagePattern = """package\s+([\w.]+)\s*;""".r
        val packageName    = packagePattern.findFirstMatchIn(cleaned).map(_.group(1))

        // Parse all top-level definitions
        var ctx = context.copy(packageName = packageName)

        // Parse enums first (they might be referenced by messages)
        val enumPattern = """enum\s+(\w+)\s*\{([^}]*)\}""".r
        for m <- enumPattern.findAllMatchIn(cleaned) do
            val enumName         = m.group(1)
            val enumBody         = m.group(2)
            val (schema, newCtx) = parseEnum(enumName, enumBody, ctx.withPath(enumName))
            ctx = newCtx.addEnum(enumName, schema)

        // Parse messages
        val messagePattern = """message\s+(\w+)\s*\{""".r
        var pos            = 0
        for m <- messagePattern.findAllMatchIn(cleaned) do
            val msgName    = m.group(1)
            val startBrace = m.end - 1
            findMatchingBrace(cleaned, startBrace) match
                case Some(endBrace) =>
                    val msgBody          = cleaned.substring(startBrace + 1, endBrace)
                    val (schema, newCtx) = parseMessage(msgName, msgBody, ctx.withPath(msgName))
                    ctx = newCtx.addMessage(msgName, schema)
                case None           =>
                    ctx = ctx.addFriction(FrictionType.Loss, s"Failed to parse message $msgName: unmatched braces")

        // Handle field number loss
        if ctx.messages.nonEmpty || ctx.enums.nonEmpty then
            ctx = ctx.addFriction(
                FrictionType.Loss,
                "Field numbers are discarded during import (ReNGBis doesn't track field ordering)"
            )

        Right(ImportResult(ctx.messages ++ ctx.enums, ctx.report))

    private def removeComments(content: String): String =
        // Remove single-line comments
        val noSingleLine = content.replaceAll("//[^\n]*", "")
        // Remove multi-line comments
        noSingleLine.replaceAll("/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/", "")

    private def findMatchingBrace(content: String, start: Int): Option[Int] =
        var depth = 1
        var i     = start + 1
        while i < content.length && depth > 0 do
            content.charAt(i) match
                case '{' => depth += 1
                case '}' => depth -= 1
                case _   =>
            i += 1
        if depth == 0 then Some(i - 1) else None

    private def parseEnum(name: String, body: String, context: TranslationContext): (Schema, TranslationContext) =
        val valuePattern = """(\w+)\s*=\s*(-?\d+)""".r
        val values       = valuePattern.findAllMatchIn(body).map(_.group(1)).toSeq

        if values.isEmpty then (EnumValues(), context.addFriction(FrictionType.Loss, s"Empty enum $name"))
        else
            // Convert SCREAMING_SNAKE_CASE to lowercase for ReNGBis
            val normalizedValues = values.map(v => v.toLowerCase.replace("_", "-"))
            (EnumValues(normalizedValues*), context)

    private def parseMessage(name: String, body: String, context: TranslationContext): (Schema, TranslationContext) =
        var ctx    = context
        val fields = scala.collection.mutable.LinkedHashMap[ObjectLabel, Schema]()

        // Parse nested enums first
        val nestedEnumPattern = """enum\s+(\w+)\s*\{([^}]*)\}""".r
        for m <- nestedEnumPattern.findAllMatchIn(body) do
            val enumName         = m.group(1)
            val enumBody         = m.group(2)
            val fullName         = s"$name.$enumName"
            val (schema, newCtx) = parseEnum(enumName, enumBody, ctx.withPath(fullName))
            ctx = newCtx.addEnum(fullName, schema)

        // Parse nested messages
        val nestedMsgPattern = """message\s+(\w+)\s*\{""".r
        for m <- nestedMsgPattern.findAllMatchIn(body) do
            val msgName    = m.group(1)
            val startBrace = m.end - 1
            findMatchingBrace(body, startBrace) match
                case Some(endBrace) =>
                    val msgBody          = body.substring(startBrace + 1, endBrace)
                    val fullName         = s"$name.$msgName"
                    val (schema, newCtx) = parseMessage(msgName, msgBody, ctx.withPath(fullName))
                    ctx = newCtx.addMessage(fullName, schema)
                case None           =>
                    ctx = ctx.addFriction(FrictionType.Loss, s"Failed to parse nested message $msgName")

        // Parse oneof blocks
        val oneofPattern = """oneof\s+(\w+)\s*\{([^}]*)\}""".r
        val oneofFields  = scala.collection.mutable.Map[String, Seq[(String, Schema)]]()
        for m <- oneofPattern.findAllMatchIn(body) do
            val oneofName       = m.group(1)
            val oneofBody       = m.group(2)
            val oneofFieldsList = parseOneofFields(oneofBody, ctx)
            oneofFields(oneofName) = oneofFieldsList._1
            ctx = oneofFieldsList._2

        // Parse regular fields (excluding those in oneof and nested definitions)
        val cleanedBody  = removeNestedDefinitions(body)
        val fieldPattern = """(optional\s+|repeated\s+)?(map<\s*(\w+)\s*,\s*([\w.]+)\s*>|[\w.]+)\s+(\w+)\s*=\s*(\d+)""".r

        for m <- fieldPattern.findAllMatchIn(cleanedBody) do
            val modifier  = Option(m.group(1)).map(_.trim)
            val fullType  = m.group(2)
            val fieldName = m.group(5)
            val fieldPath = s"${ ctx.path }.$fieldName"

            val (fieldSchema, newCtx) = if fullType.startsWith("map<") then
                val keyType   = m.group(3)
                val valueType = m.group(4)
                parseMapField(keyType, valueType, ctx.withPath(fieldPath))
            else parseFieldType(fullType, modifier, ctx.withPath(fieldPath))

            ctx = newCtx

            val label = modifier match
                case Some("optional") => OptionalLabel(toCamelCase(fieldName))
                case _                => MandatoryLabel(toCamelCase(fieldName))

            // Handle repeated fields
            val finalSchema = modifier match
                case Some("repeated") => ListOfValues(fieldSchema)
                case _                => fieldSchema

            fields(label) = finalSchema

        // Add oneof fields as alternatives
        for (oneofName, oneofFieldsList) <- oneofFields do
            if oneofFieldsList.nonEmpty then
                val alternatives = oneofFieldsList.map(_._2)
                val schema       =
                    if alternatives.size == 1 then alternatives.head
                    else AlternativeValues(alternatives*)
                fields(OptionalLabel(toCamelCase(oneofName))) = schema

        (ObjectValue(fields.toMap), ctx)

    private def parseOneofFields(body: String, context: TranslationContext): (Seq[(String, Schema)], TranslationContext) =
        var ctx    = context
        val fields = scala.collection.mutable.ArrayBuffer[(String, Schema)]()

        val fieldPattern = """([\w.]+)\s+(\w+)\s*=\s*(\d+)""".r
        for m <- fieldPattern.findAllMatchIn(body) do
            val fieldType        = m.group(1)
            val fieldName        = m.group(2)
            val (schema, newCtx) = parseFieldType(fieldType, None, ctx)
            ctx = newCtx
            fields += ((fieldName, schema))

        (fields.toSeq, ctx)

    private def removeNestedDefinitions(body: String): String =
        // Remove nested messages and enums for field parsing
        var result = body

        // Remove nested enums
        val enumPattern = """enum\s+\w+\s*\{[^}]*\}""".r
        result = enumPattern.replaceAllIn(result, "")

        // Remove nested messages (simplified - may not handle deeply nested)
        val msgPattern = """message\s+\w+\s*\{""".r
        var pos        = 0
        val sb         = StringBuilder()
        var lastEnd    = 0

        for m <- msgPattern.findAllMatchIn(result) do
            sb.append(result.substring(lastEnd, m.start))
            findMatchingBrace(result, m.end - 1) match
                case Some(endBrace) =>
                    lastEnd = endBrace + 1
                case None           =>
                    lastEnd = m.end

        sb.append(result.substring(lastEnd))

        // Remove oneof blocks (already processed separately)
        val oneofPattern = """oneof\s+\w+\s*\{[^}]*\}""".r
        oneofPattern.replaceAllIn(sb.toString(), "")

    private def parseMapField(keyType: String, valueType: String, context: TranslationContext): (Schema, TranslationContext) =
        val ctx =
            if keyType != "string" then
                context.addFriction(
                    FrictionType.Approximation,
                    s"Map key type '$keyType' converted to string (ReNGBis maps always have string keys)"
                )
            else context

        val (valueSchema, newCtx) = parseFieldType(valueType, None, ctx)
        (MapValue(valueSchema), newCtx)

    private def parseFieldType(typeName: String, modifier: Option[String], context: TranslationContext): (Schema, TranslationContext) =
        typeName match
            // Scalar types
            case "double" | "float" =>
                (NumericValue(), context)

            case "int32" | "sint32" | "sfixed32" =>
                (NumericValue(NumericConstraint.Constraints(integer = true)), context)

            case "int64" | "sint64" | "sfixed64" =>
                (NumericValue(NumericConstraint.Constraints(integer = true)), context)

            case "uint32" | "fixed32" =>
                val ctx = context.addFriction(
                    FrictionType.Approximation,
                    s"Unsigned integer type '$typeName' treated as signed integer"
                )
                (NumericValue(NumericConstraint.Constraints(integer = true)), ctx)

            case "uint64" | "fixed64" =>
                val ctx = context.addFriction(
                    FrictionType.Approximation,
                    s"Unsigned integer type '$typeName' treated as signed integer"
                )
                (NumericValue(NumericConstraint.Constraints(integer = true)), ctx)

            case "bool" =>
                (BooleanValue(), context)

            case "string" =>
                (TextValue(), context)

            case "bytes" =>
                (BinaryValue(), context)

            // Well-known types
            case "google.protobuf.Any" =>
                (AnyValue(), context)

            case "google.protobuf.Timestamp" =>
                (TimeValue(TimeConstraint.NamedFormat.ISO8601), context)

            case "google.protobuf.Duration" =>
                val ctx = context.addFriction(
                    FrictionType.Approximation,
                    "google.protobuf.Duration approximated as text"
                )
                (TextValue(), ctx)

            case "google.protobuf.Struct" =>
                val ctx = context.addFriction(
                    FrictionType.Approximation,
                    "google.protobuf.Struct approximated as map of any values"
                )
                (MapValue(AnyValue()), ctx)

            case "google.protobuf.Value" =>
                (AnyValue(), context)

            case "google.protobuf.ListValue" =>
                (ListOfValues(AnyValue()), context)

            case "google.protobuf.StringValue" | "google.protobuf.BytesValue" =>
                (TextValue(), context)

            case "google.protobuf.BoolValue" =>
                (BooleanValue(), context)

            case "google.protobuf.Int32Value" | "google.protobuf.Int64Value" | "google.protobuf.UInt32Value" | "google.protobuf.UInt64Value" =>
                (NumericValue(NumericConstraint.Constraints(integer = true)), context)

            case "google.protobuf.FloatValue" | "google.protobuf.DoubleValue" =>
                (NumericValue(), context)

            // Custom/referenced types
            case other =>
                // Check if it's a known type in context
                context.lookupType(other) match
                    case Some(schema) => (NamedValueReference(toCamelCase(other)), context)
                    case None         =>
                        // Check with parent context prefix
                        val parentPath    = context.path.split("\\.").dropRight(1).mkString(".")
                        val qualifiedName = if parentPath.nonEmpty then s"$parentPath.$other" else other
                        context.lookupType(qualifiedName) match
                            case Some(schema) => (NamedValueReference(toCamelCase(qualifiedName)), context)
                            case None         =>
                                // Assume it's a forward reference
                                (NamedValueReference(toCamelCase(other)), context)

    // Utility functions

    private def toCamelCase(s: String): String =
        // If already camelCase (no underscores/dashes and has mixed case), preserve it
        if !s.contains("_") && !s.contains("-") && s.exists(_.isUpper) && s.exists(_.isLower) then
            // Just ensure first char is lowercase
            s.headOption.map(_.toLower).getOrElse("").toString + s.drop(1)
        else
            val parts = s.split("[_\\-\\s]+")
            if parts.isEmpty then s
            else parts.head.toLowerCase + parts.tail.map(p => p.headOption.map(_.toUpper).getOrElse("").toString + p.drop(1).toLowerCase).mkString

    private def toPascalCase(s: String): String =
        s.split("[_\\-\\s]+")
            .map(word => word.headOption.map(_.toUpper).getOrElse("").toString + word.drop(1).toLowerCase)
            .mkString

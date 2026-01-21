package rengbis.translators.schemas.protobuf

import rengbis.Schema.*
import rengbis.translators.common.{ FrictionReport, FrictionType }

object ProtobufExporter:

    case class TranslationContext(
        path: String,
        report: FrictionReport,
        fieldNumber: Int = 1,
        messageCounter: Int = 0,
        enumCounter: Int = 0,
        nestedMessages: Vector[String] = Vector.empty,
        nestedEnums: Vector[String] = Vector.empty,
        indent: Int = 0
    ):
        def withPath(newPath: String): TranslationContext =
            copy(path = newPath)

        def addFriction(frictionType: FrictionType, message: String): TranslationContext =
            val updatedReport = frictionType match
                case FrictionType.Loss          => report.addLoss(path, message)
                case FrictionType.Approximation => report.addApproximation(path, message)
                case FrictionType.Extension     => report.addExtension(path, message)
            copy(report = updatedReport)

        def nextFieldNumber: (Int, TranslationContext) =
            (fieldNumber, copy(fieldNumber = fieldNumber + 1))

        def resetFieldNumber: TranslationContext =
            copy(fieldNumber = 1)

        def nextMessageName(hint: String): (String, TranslationContext) =
            val name = s"${ hint }${ messageCounter }"
            (name, copy(messageCounter = messageCounter + 1))

        def nextEnumName(hint: String): (String, TranslationContext) =
            val name = s"${ hint }${ enumCounter }"
            (name, copy(enumCounter = enumCounter + 1))

        def addNestedMessage(msg: String): TranslationContext =
            copy(nestedMessages = nestedMessages :+ msg)

        def addNestedEnum(enumDef: String): TranslationContext =
            copy(nestedEnums = nestedEnums :+ enumDef)

        def clearNested: TranslationContext =
            copy(nestedMessages = Vector.empty, nestedEnums = Vector.empty)

        def withIndent(i: Int): TranslationContext =
            copy(indent = i)

    case class ExportResult(
        proto: String,
        report: FrictionReport
    )

    /** Exports a ReNGBis schema to Protocol Buffers proto3 format. */
    def toProtobuf(
        schema: Schema,
        messageName: String = "Root",
        packageName: Option[String] = None
    ): ExportResult =
        val context                     = TranslationContext("", FrictionReport())
        val (messageBody, finalContext) = translateToMessage(schema, messageName, context)

        val sb = StringBuilder()
        sb.append("syntax = \"proto3\";\n\n")

        packageName.foreach { pkg =>
            sb.append(s"package $pkg;\n\n")
        }

        // Add google.protobuf imports if needed
        if messageBody.contains("google.protobuf.") then
            if messageBody.contains("google.protobuf.Any") then sb.append("import \"google/protobuf/any.proto\";\n")
            if messageBody.contains("google.protobuf.Timestamp") then sb.append("import \"google/protobuf/timestamp.proto\";\n")
            sb.append("\n")

        sb.append(messageBody)
        ExportResult(sb.toString().trim, finalContext.report)

    /** Exports a ReNGBis schema with named definitions to Protocol Buffers proto3 format. */
    def toProtobufWithDefinitions(
        definitions: Map[String, Schema],
        rootName: Option[String] = None,
        packageName: Option[String] = None
    ): ExportResult =
        var context  = TranslationContext("", FrictionReport())
        val messages = StringBuilder()

        // Sort definitions for consistent output
        val sortedDefs = definitions.toSeq.sortBy(_._1)

        for (name, schema) <- sortedDefs do
            val msgName                   = toPascalCase(name)
            val (messageBody, newContext) = translateToMessage(schema, msgName, context.withPath(name).resetFieldNumber)
            context = newContext
            messages.append(messageBody)
            messages.append("\n\n")

        val sb = StringBuilder()
        sb.append("syntax = \"proto3\";\n\n")

        packageName.foreach { pkg =>
            sb.append(s"package $pkg;\n\n")
        }

        val content = messages.toString()
        if content.contains("google.protobuf.") then
            if content.contains("google.protobuf.Any") then sb.append("import \"google/protobuf/any.proto\";\n")
            if content.contains("google.protobuf.Timestamp") then sb.append("import \"google/protobuf/timestamp.proto\";\n")
            sb.append("\n")

        sb.append(content.trim)
        ExportResult(sb.toString().trim, context.report)

    private def translateToMessage(schema: Schema, messageName: String, context: TranslationContext): (String, TranslationContext) =
        schema match
            case Documented(doc, innerSchema) =>
                val (body, ctx) = translateToMessage(innerSchema, messageName, context)
                val commented   = doc match
                    case Some(d) => s"// $d\n$body"
                    case None    => body
                (commented, ctx)

            case Deprecated(innerSchema) =>
                val ctx              = context.addFriction(
                    FrictionType.Approximation,
                    "Deprecated annotation added as comment; proto3 has deprecated option but it's field-level only"
                )
                val (body, finalCtx) = translateToMessage(innerSchema, messageName, ctx)
                (s"// DEPRECATED\n$body", finalCtx)

            case ObjectValue(fields) =>
                translateObjectToMessage(fields, messageName, context)

            case other =>
                // Wrap non-object schemas in a message with a single 'value' field
                val (fieldType, fieldCtx) = translateFieldType(other, "value", context)
                val ctx                   = fieldCtx.addFriction(
                    FrictionType.Approximation,
                    s"Non-object schema wrapped in message with 'value' field"
                )
                val body                  = s"""message $messageName {
  $fieldType value = 1;
}"""
                (body, ctx)

    private def translateObjectToMessage(
        fields: Map[ObjectLabel, Schema],
        messageName: String,
        context: TranslationContext
    ): (String, TranslationContext) =
        var ctx        = context.resetFieldNumber.clearNested
        val fieldLines = Vector.newBuilder[String]

        // Sort fields for consistent output
        val sortedFields = fields.toSeq.sortBy(_._1.label)

        for (label, schema) <- sortedFields do
            val fieldName          = toSnakeCase(label.label)
            val fieldPath          = if ctx.path.isEmpty then fieldName else s"${ ctx.path }.$fieldName"
            val (fieldNum, numCtx) = ctx.nextFieldNumber
            ctx = numCtx.withPath(fieldPath)

            val (fieldType, typeCtx) = translateFieldType(schema, toPascalCase(label.label), ctx)
            ctx = typeCtx

            val optionalPrefix = label match
                case OptionalLabel(_)  => "optional "
                case MandatoryLabel(_) => ""

            // Add documentation comment if present
            val docComment = schema match
                case Documented(Some(doc), _) => s"  // $doc\n"
                case _                        => ""

            // Add deprecated option if needed
            val deprecatedOption = schema match
                case Deprecated(_)                => " [deprecated = true]"
                case Documented(_, Deprecated(_)) => " [deprecated = true]"
                case _                            => ""

            fieldLines += s"$docComment  $optionalPrefix$fieldType $fieldName = $fieldNum$deprecatedOption;"

        val nestedContent = (ctx.nestedEnums ++ ctx.nestedMessages).mkString("\n\n")
        val nestedSection = if nestedContent.nonEmpty then s"\n$nestedContent\n" else ""

        val body = s"""message $messageName {$nestedSection
${ fieldLines.result().mkString("\n") }
}"""
        (body, ctx.clearNested)

    private def translateFieldType(schema: Schema, nameHint: String, context: TranslationContext): (String, TranslationContext) =
        schema match
            case Documented(_, innerSchema) =>
                translateFieldType(innerSchema, nameHint, context)

            case Deprecated(innerSchema) =>
                translateFieldType(innerSchema, nameHint, context)

            case AnyValue() =>
                (s"google.protobuf.Any", context)

            case BooleanValue(_) =>
                ("bool", context)

            case TextValue(constraints, _) =>
                val ctx =
                    if !constraints.isEmpty then
                        context.addFriction(
                            FrictionType.Loss,
                            s"Text constraints cannot be represented in Protocol Buffers"
                        )
                    else context
                ("string", ctx)

            case GivenTextValue(value) =>
                val ctx = context.addFriction(
                    FrictionType.Loss,
                    s"Constant value '$value' cannot be represented in Protocol Buffers, using string"
                )
                ("string", ctx)

            case NumericValue(constraints, _) =>
                val numType = if constraints.integer then
                    // Check if we need 64-bit
                    val needsLong = constraints.value.exists { range =>
                        range.min.exists(b => b.value.abs > Int.MaxValue) ||
                        range.max.exists(b => b.value.abs > Int.MaxValue)
                    }
                    if needsLong then "int64" else "int32"
                else "double"

                val ctx =
                    if constraints.value.isDefined then
                        context.addFriction(
                            FrictionType.Loss,
                            "Numeric range constraints cannot be represented in Protocol Buffers"
                        )
                    else context

                (numType, ctx)

            case BinaryValue(constraints) =>
                val ctx =
                    if !constraints.isEmpty then
                        context.addFriction(
                            FrictionType.Loss,
                            s"Binary constraints cannot be represented in Protocol Buffers"
                        )
                    else context
                ("bytes", ctx)

            case TimeValue(constraints*) =>
                // Use google.protobuf.Timestamp for time values
                val ctx = context.addFriction(
                    FrictionType.Approximation,
                    "Time value mapped to google.protobuf.Timestamp"
                )
                ("google.protobuf.Timestamp", ctx)

            case EnumValues(values*) =>
                // Create a nested enum
                val (enumName, ctx1) = context.nextEnumName(nameHint)
                val enumValues       = values.zipWithIndex
                    .map { case (v, i) =>
                        s"    ${ toScreamingSnakeCase(v) } = $i;"
                    }
                    .mkString("\n")
                val enumDef          = s"""  enum $enumName {
$enumValues
  }"""
                val ctx2             = ctx1.addNestedEnum(enumDef)
                (enumName, ctx2)

            case ListOfValues(itemSchema, constraints) =>
                val ctx1 =
                    if !constraints.isEmpty then
                        context.addFriction(
                            FrictionType.Loss,
                            "List constraints (size, uniqueness) cannot be represented in Protocol Buffers"
                        )
                    else context

                val (itemType, ctx2) = translateFieldType(itemSchema, nameHint + "Item", ctx1)
                (s"repeated $itemType", ctx2)

            case MapValue(valueSchema) =>
                val (valueType, ctx) = translateFieldType(valueSchema, nameHint + "Value", context)
                (s"map<string, $valueType>", ctx)

            case TupleValue(items*) =>
                // Convert tuple to a nested message with field0, field1, etc.
                val (msgName, ctx1) = context.nextMessageName(nameHint)
                var ctx             = ctx1.resetFieldNumber
                val fields          = items.zipWithIndex.map { case (itemSchema, idx) =>
                    val (fieldNum, numCtx)   = ctx.nextFieldNumber
                    val (fieldType, typeCtx) = translateFieldType(itemSchema, s"Field$idx", numCtx)
                    ctx = typeCtx
                    s"    $fieldType field$idx = $fieldNum;"
                }
                val msgDef          = s"""  message $msgName {
${ fields.mkString("\n") }
  }"""
                val ctx2            = ctx
                    .addNestedMessage(msgDef)
                    .addFriction(
                        FrictionType.Approximation,
                        "Tuple converted to message with field0, field1, etc."
                    )
                (msgName, ctx2)

            case AlternativeValues(options*) =>
                // Check if this is a simple string enum (all GivenTextValue)
                val allStrings = options.forall:
                    case GivenTextValue(_) => true
                    case _                 => false

                if allStrings then
                    // Convert to protobuf enum
                    val values           = options.collect { case GivenTextValue(v) => v }
                    val (enumName, ctx1) = context.nextEnumName(nameHint)
                    val enumValues       = values.zipWithIndex
                        .map { case (v, i) =>
                            s"    ${ toScreamingSnakeCase(v) } = $i;"
                        }
                        .mkString("\n")
                    val enumDef          = s"""  enum $enumName {
$enumValues
  }"""
                    val ctx2             = ctx1.addNestedEnum(enumDef)
                    (enumName, ctx2)
                else
                    // Use oneof for complex alternatives
                    val (msgName, ctx1) = context.nextMessageName(nameHint)
                    var ctx             = ctx1.resetFieldNumber
                    val oneofFields     = options.zipWithIndex.map { case (opt, idx) =>
                        val fieldName            = s"option$idx"
                        val (fieldNum, numCtx)   = ctx.nextFieldNumber
                        val (fieldType, typeCtx) = translateFieldType(opt, s"Option$idx", numCtx)
                        ctx = typeCtx
                        s"      $fieldType $fieldName = $fieldNum;"
                    }
                    val msgDef          = s"""  message $msgName {
    oneof value {
${ oneofFields.mkString("\n") }
    }
  }"""
                    val ctx2            = ctx
                        .addNestedMessage(msgDef)
                        .addFriction(
                            FrictionType.Approximation,
                            "Alternative values converted to message with oneof"
                        )
                    (msgName, ctx2)

            case ObjectValue(fields) =>
                // Create a nested message
                val (msgName, ctx1) = context.nextMessageName(nameHint)
                val (msgBody, ctx2) = translateObjectToMessage(fields, msgName, ctx1)
                // Indent the message body for nesting
                val indentedBody    = "  " + msgBody.replace("\n", "\n  ")
                val ctx3            = ctx2.addNestedMessage(indentedBody)
                (msgName, ctx3)

            case NamedValueReference(name) =>
                (toPascalCase(name), context)

            case ScopedReference(namespace, name) =>
                val fullName = if name.isEmpty then toPascalCase(namespace) else s"${ toPascalCase(namespace) }.${ toPascalCase(name) }"
                (fullName, context)

            case Fail() =>
                val ctx = context.addFriction(
                    FrictionType.Loss,
                    "Fail type not representable in Protocol Buffers, using string"
                )
                ("string", ctx)

            case ImportStatement(_, _) =>
                val ctx = context.addFriction(
                    FrictionType.Loss,
                    "Import statements not directly supported in field context"
                )
                ("string", ctx)

    // Utility functions for naming conventions

    private def toPascalCase(s: String): String =
        s.split("[_\\-\\s]+")
            .map(word => word.headOption.map(_.toUpper).getOrElse("").toString + word.drop(1))
            .mkString

    private def toSnakeCase(s: String): String =
        s.replaceAll("([a-z])([A-Z])", "$1_$2")
            .replaceAll("[\\-\\s]+", "_")
            .toLowerCase

    private def toScreamingSnakeCase(s: String): String =
        // Handle special characters in enum values
        val cleaned = s.replaceAll("[^a-zA-Z0-9_]", "_")
        val result  = cleaned
            .replaceAll("([a-z])([A-Z])", "$1_$2")
            .replaceAll("[\\-\\s]+", "_")
            .toUpperCase
        // Protobuf enum values can't start with a number
        if result.headOption.exists(_.isDigit) then s"VALUE_$result"
        else result

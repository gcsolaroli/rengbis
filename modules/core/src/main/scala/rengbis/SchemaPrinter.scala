package rengbis

import rengbis.Schema.*

val indentation: Int = 4;

/** Configuration for schema pretty-printing. */
case class PrintConfig(
    indent: Int = indentation,           // spaces per indent level
    maxLineWidth: Int = 100,             // target max line width (soft limit)
    expandObjects: Boolean = true,       // put each object field on its own line
    expandAlternatives: Boolean = false, // put each alternative on its own line
    expandTuples: Boolean = false,       // put each tuple element on its own line
    showEmptyConstraints: Boolean = false // show [] even when no constraints
)

object PrintConfig:
    val compact: PrintConfig = PrintConfig(
        indent = 0,
        maxLineWidth = Int.MaxValue,
        expandObjects = false,
        expandAlternatives = false,
        expandTuples = false
    )

    val pretty: PrintConfig = PrintConfig(
        indent = indentation,
        maxLineWidth = 100,
        expandObjects = true,
        expandAlternatives = false,
        expandTuples = false
    )

    val expanded: PrintConfig = PrintConfig(
        indent = indentation,
        maxLineWidth = 80,
        expandObjects = true,
        expandAlternatives = true,
        expandTuples = true
    )

/** Pretty-printer for ReNGBis schemas with configurable formatting. */
object SchemaPrinter:

    /** Prints a schema with the given configuration. */
    def print(schema: Schema, config: PrintConfig = PrintConfig.pretty): String =
        printSchema(schema, 0, config)

    /** Prints a schema in compact single-line format. */
    def printCompact(schema: Schema): String =
        print(schema, PrintConfig.compact)

    /** Prints a schema in expanded multi-line format. */
    def printPretty(schema: Schema): String =
        print(schema, PrintConfig.pretty)

    /** Prints a full schema file with definitions and root schema. */
    def printWithDefinitions(
        root: Schema,
        definitions: Map[String, Schema],
        config: PrintConfig = PrintConfig.pretty
    ): String =
        val sb = StringBuilder()

        // Print definitions first
        if definitions.nonEmpty then
            val sortedDefs = definitions.toSeq.sortBy(_._1)
            for (name, schema) <- sortedDefs do
                val schemaStr = printSchema(schema, 0, config)
                sb.append(s"$name = $schemaStr")
                sb.append("\n\n")

        // Print root schema
        sb.append(s"= ${ printSchema(root, 0, config) }")

        sb.toString()

    // ========================================================================
    // Private implementation
    // ========================================================================

    private def printSchema(schema: Schema, depth: Int, config: PrintConfig): String =
        schema match
            case Documented(doc, inner)   => printDocumented(doc, inner, depth, config)
            case Deprecated(inner)        => s"@deprecated ${ printSchema(inner, depth, config) }"
            case AnyValue()               => "any"
            case Fail()                   => "fail"
            case bv: BooleanValue         => printBooleanValue(bv)
            case tv: TextValue            => printTextValue(tv)
            case gtv: GivenTextValue      => printGivenTextValue(gtv)
            case nv: NumericValue         => printNumericValue(nv)
            case bv: BinaryValue          => printBinaryValue(bv)
            case tv: TimeValue            => printTimeValue(tv)
            case ov: ObjectValue          => printObjectValue(ov, depth, config)
            case mv: MapValue             => printMapValue(mv, depth, config)
            case lv: ListOfValues         => printListOfValues(lv, depth, config)
            case tv: TupleValue           => printTupleValue(tv, depth, config)
            case av: AlternativeValues    => printAlternativeValues(av, depth, config)
            case ev: EnumValues           => printEnumValues(ev, depth, config)
            case nvr: NamedValueReference => nvr.reference
            case sr: ScopedReference      => s"${ sr.namespace }.${ sr.name }"
            case is: ImportStatement      => s"${ is.namespace } => import ${ quoteIfNeeded(is.path) }"

    private def printBooleanValue(bv: BooleanValue): String =
        bv.default match
            case Some(d) => s"boolean ?= $d"
            case None    => "boolean"

    private def printDocumented(doc: Option[String], inner: Schema, depth: Int, config: PrintConfig): String =
        val innerStr = printSchema(inner, depth, config)
        doc match
            case Some(text) if config.expandObjects =>
                val docLines = text.split("\n").map(line => s"## $line").mkString("\n")
                s"$docLines\n${ indent(depth, config) }$innerStr"
            case Some(text)                         =>
                // Inline doc comment for compact mode
                s"$innerStr ## ${ text.split("\n").head }"
            case None                               => innerStr

    private def printTextValue(tv: TextValue): String =
        val constraints = printTextConstraints(tv.constraints)
        val default     = tv.default.map(d => s" ?= ${ quote(d) }").getOrElse("")
        s"text$constraints$default"

    private def printTextConstraints(c: TextConstraint.Constraints): String =
        if c.isEmpty then ""
        else
            val parts = Seq(
                c.size.map(printTextSizeRange("length", _)),
                c.regex.map(r => s"regex = ${ quote(r) }"),
                c.format.map(f => s"pattern = ${ quote(f) }")
            ).flatten
            s" [ ${ parts.mkString(", ") } ]"

    private def printGivenTextValue(gtv: GivenTextValue): String =
        quote(gtv.value)

    private def printNumericValue(nv: NumericValue): String =
        val constraints = printNumericConstraints(nv.constraints)
        val default     = nv.default.map(d => s" ?= $d").getOrElse("")
        s"number$constraints$default"

    private def printNumericConstraints(c: NumericConstraint.Constraints): String =
        if c.isEmpty then ""
        else
            val parts = Seq(
                c.value.map(printValueRange),
                if c.integer then Some("integer") else None
            ).flatten
            s" [ ${ parts.mkString(", ") } ]"

    private def printBinaryValue(bv: BinaryValue): String =
        val constraints = printBinaryConstraints(bv.constraints)
        s"binary$constraints"

    private def printBinaryConstraints(c: BinaryConstraint.Constraints): String =
        if c.isEmpty then ""
        else
            val parts = Seq(
                c.size.map(printBinarySizeRange),
                c.encoding.map(e => s"encoding = '${ e.code }'")
            ).flatten
            s" [ ${ parts.mkString(", ") } ]"

    private def printTimeValue(tv: TimeValue): String =
        if tv.constraints.isEmpty then "time"
        else
            val formats = tv.constraints.map:
                case nf: TimeConstraint.NamedFormat   => s"format = '${ formatName(nf) }'"
                case cp: TimeConstraint.CustomPattern => s"format = ${ quote(cp.pattern) }"
            s"time [ ${ formats.mkString(", ") } ]"

    private def formatName(nf: TimeConstraint.NamedFormat): String = nf match
        case TimeConstraint.NamedFormat.ISO8601          => "iso8601"
        case TimeConstraint.NamedFormat.ISO8601_DateTime => "iso8601-datetime"
        case TimeConstraint.NamedFormat.ISO8601_Date     => "iso8601-date"
        case TimeConstraint.NamedFormat.ISO8601_Time     => "iso8601-time"
        case TimeConstraint.NamedFormat.RFC3339          => "rfc3339"

    private def printObjectValue(ov: ObjectValue, depth: Int, config: PrintConfig): String =
        if ov.obj.isEmpty then "{ }"
        else if config.expandObjects then printExpandedObject(ov, depth, config)
        else printCompactObject(ov, depth, config)

    private def printExpandedObject(ov: ObjectValue, depth: Int, config: PrintConfig): String =
        val innerDepth    = depth + 1
        val closingIndent = indent(depth, config)

        // printObjectField returns fully indented content (including multi-line comments)
        val fields = ov.obj.toSeq.map { (label, schema) =>
            printObjectField(label, schema, innerDepth, config)
        }

        s"{\n${ fields.mkString(",\n") }\n$closingIndent}"

    /** Prints an object field, handling:
      *   - Optional fields with defaults become mandatory with default
      *   - Simple types with documentation use trailing comments
      */
    private def printObjectField(label: ObjectLabel, schema: Schema, depth: Int, config: PrintConfig): String =
        // Extract documentation and inner schema
        val (doc, deprecatedFlag, innerSchema) = unwrapMetadata(schema)

        // Determine if this is a simple type that can use trailing comment
        val isSimpleType = innerSchema match
            case _: BooleanValue | _: TextValue | _: NumericValue | _: TimeValue | _: GivenTextValue | _: EnumValues | _: NamedValueReference => true
            case _                                                                                                                            => false

        // Check if inner schema has a default
        val hasDefault = innerSchema match
            case BooleanValue(Some(_))    => true
            case TextValue(_, Some(_))    => true
            case NumericValue(_, Some(_)) => true
            case _                        => false

        // Adjust label: optional with default becomes mandatory
        val adjustedLabel = (label, hasDefault) match
            case (OptionalLabel(name), true) => MandatoryLabel(name)
            case _                           => label

        val labelStr   = printLabel(adjustedLabel)
        val schemaStr  = printSchema(innerSchema, depth, config)
        val deprecated = if deprecatedFlag then "@deprecated " else ""

        val indentStr = indent(depth, config)

        // Format with trailing comment for simple types, preceding for complex
        (doc, isSimpleType && config.expandObjects) match
            case (Some(text), true)  =>
                // Simple type: use trailing comment (single line)
                val shortDoc = text.split("\n").head
                s"$indentStr$deprecated$labelStr: $schemaStr  ## $shortDoc"
            case (Some(text), false) =>
                // Complex type: use preceding comment with proper indentation on each line
                val docLines = text.split("\n").map(line => s"$indentStr## $line").mkString("\n")
                s"$docLines\n$indentStr$deprecated$labelStr: $schemaStr"
            case (None, _)           =>
                s"$indentStr$deprecated$labelStr: $schemaStr"

    /** Unwraps Documented and Deprecated wrappers from a schema */
    private def unwrapMetadata(schema: Schema): (Option[String], Boolean, Schema) =
        schema match
            case Deprecated(Documented(doc, inner)) => (doc, true, inner)
            case Documented(doc, Deprecated(inner)) => (doc, true, inner)
            case Deprecated(inner)                  => (None, true, inner)
            case Documented(doc, inner)             => (doc, false, inner)
            case other                              => (None, false, other)

    private def printCompactObject(ov: ObjectValue, depth: Int, config: PrintConfig): String =
        val fields = ov.obj.toSeq.map { (label, schema) =>
            val labelStr  = printLabel(label)
            val schemaStr = printSchema(schema, depth, config)
            s"$labelStr: $schemaStr"
        }
        s"{ ${ fields.mkString(", ") } }"

    private def printLabel(label: ObjectLabel): String = label match
        case MandatoryLabel(name) => name
        case OptionalLabel(name)  => s"$name?"

    private def printMapValue(mv: MapValue, depth: Int, config: PrintConfig): String =
        val valueStr = printSchema(mv.valueSchema, depth, config)
        s"{ ...: $valueStr }"

    private def printListOfValues(lv: ListOfValues, depth: Int, config: PrintConfig): String =
        val inner       = printSchema(lv.schema, depth, config)
        val needsParens = lv.schema match
            case _: AlternativeValues => true
            case _: EnumValues        => true
            case _                    => false

        val innerStr = if needsParens then s"($inner)" else inner

        val (suffix, extraConstraints) = lv.constraints.size match
            case Some(sr) if sr.min.exists(b => b.isMinInclusive && b.value == 1) && sr.max.isEmpty =>
                // Just min >= 1, use + suffix
                ("+", lv.constraints.copy(size = None))
            case _                                                                                  =>
                ("*", lv.constraints)

        val constraintStr = printListConstraints(extraConstraints)
        s"$innerStr$suffix$constraintStr"

    private def printListConstraints(c: ListConstraint.Constraints): String =
        if c.isEmpty then ""
        else
            val parts = Seq(
                c.size.map(printListSizeRange("size", _))
            ).flatten ++ c.unique.map(printUniqueness)
            s" [ ${ parts.mkString(", ") } ]"

    private def printUniqueness(u: ListConstraint.Uniqueness): String = u match
        case ListConstraint.Uniqueness.Simple           => "unique"
        case ListConstraint.Uniqueness.ByFields(Seq(f)) => s"unique = $f"
        case ListConstraint.Uniqueness.ByFields(fs)     => s"unique = (${ fs.mkString(", ") })"

    private def printTupleValue(tv: TupleValue, depth: Int, config: PrintConfig): String =
        if config.expandTuples && tv.options.size > 2 then printExpandedTuple(tv, depth, config)
        else printCompactTuple(tv, depth, config)

    private def printExpandedTuple(tv: TupleValue, depth: Int, config: PrintConfig): String =
        val innerDepth    = depth + 1
        val indentStr     = indent(innerDepth, config)
        val closingIndent = indent(depth, config)

        val elements = tv.options.map { schema =>
            s"$indentStr${ printSchema(schema, innerDepth, config) }"
        }

        s"(\n${ elements.mkString(",\n") }\n$closingIndent)"

    private def printCompactTuple(tv: TupleValue, depth: Int, config: PrintConfig): String =
        val elements = tv.options.map(s => printSchema(s, depth, config))
        s"(${ elements.mkString(", ") })"

    private def printAlternativeValues(av: AlternativeValues, depth: Int, config: PrintConfig): String =
        if config.expandAlternatives && av.options.size > 2 then printExpandedAlternatives(av, depth, config)
        else printCompactAlternatives(av, depth, config)

    private def printExpandedAlternatives(av: AlternativeValues, depth: Int, config: PrintConfig): String =
        val innerDepth = depth + 1
        val indentStr  = indent(innerDepth, config)

        val options = av.options.map { schema =>
            s"$indentStr| ${ printSchema(schema, innerDepth, config) }"
        }

        // First option without leading |
        val firstOption = s"${ indent(innerDepth, config) }${ printSchema(av.options.head, innerDepth, config) }"
        val restOptions = av.options.tail.map { schema =>
            s"$indentStr| ${ printSchema(schema, innerDepth, config) }"
        }

        s"(\n$firstOption\n${ restOptions.mkString("\n") }\n${ indent(depth, config) })"

    private def printCompactAlternatives(av: AlternativeValues, depth: Int, config: PrintConfig): String =
        val options = av.options.map(s => printSchema(s, depth, config))
        options.mkString(" | ")

    private def printEnumValues(ev: EnumValues, depth: Int, config: PrintConfig): String =
        val values = ev.values.map(quote)
        if config.expandAlternatives && ev.values.size > 3 then
            val innerDepth = depth + 1
            val indentStr  = indent(innerDepth, config)
            val firstValue = s"$indentStr${ values.head }"
            val restValues = values.tail.map(v => s"$indentStr| $v")
            s"(\n$firstValue\n${ restValues.mkString("\n") }\n${ indent(depth, config) })"
        else values.mkString(" | ")

    // ========================================================================
    // Constraint printing helpers
    // ========================================================================

    private def printTextSizeRange(key: String, sr: TextConstraint.SizeRange): String =
        printBoundedRange(key, sr.min, sr.max, _.toString)

    private def printListSizeRange(key: String, sr: ListConstraint.SizeRange): String =
        printBoundedRange(key, sr.min, sr.max, _.toString)

    private def printValueRange(vr: NumericConstraint.ValueRange): String =
        printBoundedRange("value", vr.min, vr.max, _.toString)

    private def printBinarySizeRange(sr: BinaryConstraint.SizeRange): String =
        printBoundedRange("bytes", sr.min, sr.max, _.toString)

    private def printBoundedRange[V](
        key: String,
        min: Option[BoundConstraint[V]],
        max: Option[BoundConstraint[V]],
        formatValue: V => String
    ): String =
        (min, max) match
            case (Some(minB), Some(maxB)) if minB.op == BoundOp.Exact =>
                s"$key == ${ formatValue(minB.value) }"
            case (Some(minB), Some(maxB))                             =>
                // Range: value op key op value
                val minOp = if minB.op == BoundOp.MinInclusive then "<=" else "<"
                val maxOp = if maxB.op == BoundOp.MaxInclusive then "<=" else "<"
                s"${ formatValue(minB.value) } $minOp $key $maxOp ${ formatValue(maxB.value) }"
            case (Some(minB), None)                                   =>
                val op = minB.op match
                    case BoundOp.MinInclusive => ">="
                    case BoundOp.MinExclusive => ">"
                    case BoundOp.Exact        => "=="
                    case _                    => ">="
                s"$key $op ${ formatValue(minB.value) }"
            case (None, Some(maxB))                                   =>
                val op = maxB.op match
                    case BoundOp.MaxInclusive => "<="
                    case BoundOp.MaxExclusive => "<"
                    case _                    => "<="
                s"$key $op ${ formatValue(maxB.value) }"
            case (None, None)                                         =>
                key // shouldn't happen, but safe fallback

    // ========================================================================
    // Utility functions
    // ========================================================================

    private def indent(depth: Int, config: PrintConfig): String =
        if config.indent == 0 then ""
        else " " * (depth * config.indent)

    private def quote(s: String): String =
        // TODO: proper escaping
        s"\"$s\""

    private def quoteIfNeeded(s: String): String =
        if s.contains(' ') || s.contains('\t') then quote(s)
        else s

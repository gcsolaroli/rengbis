/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

/**
 * Tree-sitter grammar for ReNGBis schema language
 *
 * ReNGBis is a content schema definition language for validating payloads.
 */

module.exports = grammar({
    name: "rengbis",

    extras: ($) => [/[ \t\r]/, $.comment],

    word: ($) => $.identifier,

    conflicts: ($) => [
        [$._items, $._item],
        [$.object_fields],
        [$.object_value],
        [$.object_fields, $._field_sep],
    ],

    rules: {
        // ========================================================================
        // Top-level schema
        // ========================================================================

        schema: ($) =>
            seq(
                repeat($._nl),
                repeat(seq($.definition, repeat1($._nl))),
                optional(seq($.root, repeat($._nl))),
            ),

        _nl: ($) => /\n/,

        root: ($) =>
            seq(
                optional($.doc_comment),
                "=",
                $._items,
                optional($.trailing_doc_comment),
            ),

        definition: ($) => choice($.import_statement, $.named_value),

        // ========================================================================
        // Comments
        // ========================================================================

        comment: ($) => token(seq("#", optional(seq(/[^#\n]/, /[^\n]*/)))),

        doc_comment: ($) => repeat1($.doc_comment_line),

        doc_comment_line: ($) => seq("##", /[^\n]*/, "\n"),

        trailing_doc_comment: ($) => seq("##", /[^\n]*/),

        // ========================================================================
        // Imports
        // ========================================================================

        import_statement: ($) =>
            seq(
                field("namespace", $.identifier),
                "=>",
                "import",
                field("path", $.import_path),
            ),

        import_path: ($) => choice($.quoted_string, /[^ \t\r\n]+/),

        // ========================================================================
        // Named values (definitions)
        // ========================================================================

        named_value: ($) =>
            prec.right(
                seq(
                    optional($.doc_comment),
                    optional(seq($.deprecated_marker, repeat($._nl))),
                    field("name", $.identifier),
                    "=",
                    field("value", $._items),
                    optional($.trailing_doc_comment),
                ),
            ),

        deprecated_marker: ($) => "@deprecated",

        // ========================================================================
        // Items (value expressions)
        // ========================================================================

        _items: ($) =>
            choice(
                $.alternative_values,
                $.list_of_values,
                $.tuple_value,
                $._item,
            ),

        _item: ($) =>
            choice(
                $.any_value,
                $.boolean_value,
                $.numeric_value,
                $.binary_value,
                $.time_value,
                $.text_value,
                $.given_text_value,
                $.map_value,
                $.object_value,
                $.grouped_alternatives,
                $.tuple_value,
                $.scoped_reference,
                $.named_reference,
            ),

        // ========================================================================
        // Primitive types
        // ========================================================================

        any_value: ($) => "any",

        boolean_value: ($) => "boolean",

        // Text value: text[constraints] ?= "default"
        text_value: ($) =>
            seq(
                "text",
                optional($.text_constraint_block),
                optional($.text_default),
            ),

        text_constraint_block: ($) =>
            seq("[", commaSep1($.text_constraint), "]"),

        text_constraint: ($) =>
            choice(
                $.length_constraint,
                $.regex_constraint,
                $.pattern_constraint,
            ),

        length_constraint: ($) =>
            choice(
                // Form: length >= 5
                seq("length", $.comparison_op, $.integer),
                // Form: 5 <= length <= 10 (range)
                seq(
                    $.integer,
                    $.comparison_op,
                    "length",
                    $.comparison_op,
                    $.integer,
                ),
                // Form: 5 <= length
                seq($.integer, $.comparison_op, "length"),
            ),

        regex_constraint: ($) => seq("regex", "=", $.quoted_string),

        pattern_constraint: ($) => seq("pattern", "=", $.quoted_string),

        text_default: ($) => seq("?=", $.quoted_string),

        // Numeric value: number[constraints] ?= default
        numeric_value: ($) =>
            seq(
                "number",
                optional($.numeric_constraint_block),
                optional($.numeric_default),
            ),

        numeric_constraint_block: ($) =>
            seq("[", commaSep1($.numeric_constraint), "]"),

        numeric_constraint: ($) =>
            choice($.integer_constraint, $.value_constraint),

        integer_constraint: ($) => "integer",

        value_constraint: ($) =>
            choice(
                // Form: value >= 5
                seq("value", $.comparison_op, $.decimal_number),
                // Form: 5 <= value <= 10 (range)
                seq(
                    $.decimal_number,
                    $.comparison_op,
                    "value",
                    $.comparison_op,
                    $.decimal_number,
                ),
                // Form: 5 <= value
                seq($.decimal_number, $.comparison_op, "value"),
            ),

        numeric_default: ($) => seq("?=", $.decimal_number),

        // Binary value: binary[constraints]
        binary_value: ($) => seq("binary", optional($.binary_constraint_block)),

        binary_constraint_block: ($) =>
            seq("[", commaSep1($.binary_constraint), "]"),

        binary_constraint: ($) =>
            choice($.encoding_constraint, $.binary_size_constraint),

        encoding_constraint: ($) => seq("encoding", "=", $.binary_encoding),

        binary_encoding: ($) =>
            choice("'hex'", "'base64'", "'base32'", "'base58'", "'ascii85'"),

        binary_size_constraint: ($) =>
            choice(
                // Form: bytes >= 5
                seq($.binary_unit, $.comparison_op, $.integer),
                // Form: 5 <= bytes <= 10 (range)
                seq(
                    $.integer,
                    $.comparison_op,
                    $.binary_unit,
                    $.comparison_op,
                    $.integer,
                ),
                // Form: 5 <= bytes
                seq($.integer, $.comparison_op, $.binary_unit),
            ),

        binary_unit: ($) => choice("bytes", "bits", "KB", "MB", "GB"),

        // Time value: time[constraints]
        time_value: ($) => seq("time", optional($.time_constraint_block)),

        time_constraint_block: ($) =>
            seq("[", commaSep1($.time_constraint), "]"),

        time_constraint: ($) => seq("format", "=", $.time_format),

        time_format: ($) =>
            choice(
                $.named_time_format,
                $.quoted_string, // custom pattern
            ),

        named_time_format: ($) =>
            choice(
                "'iso8601'",
                "'iso8601-datetime'",
                "'iso8601-date'",
                "'iso8601-time'",
                "'rfc3339'",
            ),

        // Given text value (string literal as schema)
        given_text_value: ($) => $.quoted_string,

        // ========================================================================
        // Object and Map
        // ========================================================================

        object_value: ($) =>
            prec.right(
                seq(
                    "{",
                    optional($.trailing_doc_comment),
                    repeat($._nl),
                    optional($.object_fields),
                    repeat($._nl),
                    "}",
                ),
            ),

        object_fields: ($) =>
            seq(
                $.object_field,
                repeat(seq($._field_sep, $.object_field)),
                optional(","),
            ),

        _field_sep: ($) =>
            prec.left(choice(seq(",", repeat($._nl)), repeat1($._nl))),

        object_field: ($) =>
            seq(
                optional($.doc_comment),
                optional($.deprecated_marker),
                field("label", $.object_label),
                ":",
                field("value", $._items),
                optional($.trailing_doc_comment),
            ),

        object_label: ($) => choice($.optional_label, $.mandatory_label),

        mandatory_label: ($) => $.identifier,

        optional_label: ($) => seq($.identifier, "?"),

        map_value: ($) =>
            seq("{", $.spread_key, ":", field("value", $._items), "}"),

        spread_key: ($) => choice("...", "\u2026"), // ... or ellipsis character

        // ========================================================================
        // List, Tuple, Alternatives
        // ========================================================================

        list_of_values: ($) =>
            seq($._item, $.list_marker, optional($.list_constraint_block)),

        list_marker: ($) => choice("+", "*"),

        list_constraint_block: ($) =>
            seq("[", commaSep1($.list_constraint), "]"),

        list_constraint: ($) =>
            choice(
                $.size_constraint,
                $.unique_constraint,
                $.unique_by_field_constraint,
                $.unique_by_fields_constraint,
            ),

        size_constraint: ($) =>
            choice(
                // Form: size >= 5
                seq("size", $.comparison_op, $.integer),
                // Form: 5 <= size <= 10 (range)
                seq(
                    $.integer,
                    $.comparison_op,
                    "size",
                    $.comparison_op,
                    $.integer,
                ),
                // Form: 5 <= size
                seq($.integer, $.comparison_op, "size"),
            ),

        unique_constraint: ($) => "unique",

        unique_by_field_constraint: ($) => seq("unique", "=", $.identifier),

        unique_by_fields_constraint: ($) =>
            seq("unique", "=", "(", commaSep1($.identifier), ")"),

        tuple_value: ($) => seq("(", $._item, repeat1(seq(",", $._item)), ")"),

        alternative_values: ($) =>
            seq(
                choice($.list_of_values, $._item),
                repeat1(seq("|", choice($.list_of_values, $._item))),
            ),

        grouped_alternatives: ($) => seq("(", $.alternative_values, ")"),

        // ========================================================================
        // References
        // ========================================================================

        scoped_reference: ($) =>
            seq(
                field("namespace", $.identifier),
                ".",
                field("name", $.identifier),
            ),

        named_reference: ($) => $.identifier,

        // ========================================================================
        // Operators and literals
        // ========================================================================

        comparison_op: ($) => choice("==", ">=", "<=", ">", "<"),

        identifier: ($) => /[a-zA-Z][a-zA-Z0-9_-]*/,

        integer: ($) => /[0-9]+/,

        decimal_number: ($) => /-?[0-9]+(\.[0-9]+)?/,

        quoted_string: ($) => seq('"', /[^"]*/, '"'),
    },
});

/**
 * Creates a rule to match one or more occurrences separated by commas
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function commaSep1(rule) {
    return seq(rule, repeat(seq(",", rule)));
}

/**
 * Creates a rule to match zero or more occurrences separated by commas
 * @param {RuleOrLiteral} rule
 * @returns {ChoiceRule}
 */
function commaSep(rule) {
    return optional(commaSep1(rule));
}

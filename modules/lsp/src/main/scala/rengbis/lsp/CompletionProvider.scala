package rengbis.lsp

import org.eclipse.lsp4j.*

class CompletionProvider:

    def provideCompletions(text: String, position: Position): List[CompletionItem] =
        val line      = position.getLine()
        val character = position.getCharacter()

        val lines = text.split("\n")
        if line >= 0 && line < lines.length then
            val currentLine = lines(line).take(character)
            val context     = detectContext(currentLine)

            getCompletionsForContext(context)
        else List.empty

    private enum CompletionContext:
        case TopLevel         // At root level, can suggest type keywords
        case AfterEquals      // After "=", suggest schema types
        case InsideObject     // Inside { }, suggest field syntax
        case AfterColon       // After field name ":", suggest types
        case InsideConstraint // Inside constraints { }, suggest constraint keywords    =>  WTF!!! 🤔; constraints use square brackets! [ ]
        case Unknown

    private def detectContext(linePrefix: String): CompletionContext =
        val trimmed = linePrefix.trim

        if trimmed.endsWith("=") then CompletionContext.AfterEquals
        else if trimmed.endsWith(":") then CompletionContext.AfterColon
        else if trimmed.contains("{") && !trimmed.contains("}") then
            if isInsideConstraintBlock(trimmed)
            then CompletionContext.InsideConstraint
            else CompletionContext.InsideObject
        else if trimmed.isEmpty || trimmed.startsWith("#") then CompletionContext.TopLevel
        else CompletionContext.Unknown

    private def isInsideConstraintBlock(line: String): Boolean =
        List("text", "number", "boolean", "any").exists(kw => line.contains(kw)) && line.contains("{")

    private def getCompletionsForContext(context: CompletionContext): List[CompletionItem] =
        context match
            case CompletionContext.TopLevel | CompletionContext.AfterEquals | CompletionContext.AfterColon =>
                typeCompletions ++ structureCompletions
            case CompletionContext.InsideObject                                                            =>
                objectFieldCompletions
            case CompletionContext.InsideConstraint                                                        =>
                constraintCompletions
            case CompletionContext.Unknown                                                                 =>
                allCompletions

    private def typeCompletions: List[CompletionItem] = List(
        createCompletion("text", CompletionItemKind.Keyword, "String value type", "text"),
        createCompletion("number", CompletionItemKind.Keyword, "Numeric value type", "number"),
        createCompletion("boolean", CompletionItemKind.Keyword, "Boolean value type", "boolean"),
        createCompletion("any", CompletionItemKind.Keyword, "Any value type", "any")
    )

    private def structureCompletions: List[CompletionItem] = List(
        createCompletion("{ }", CompletionItemKind.Snippet, "Object with fields", "{ $1: $2 }"),
        createCompletion("[ ]", CompletionItemKind.Snippet, "Tuple", "[ $1, $2 ]"),
        createCompletion("text*", CompletionItemKind.Snippet, "List (zero or more)", "$1*"),
        createCompletion("text+", CompletionItemKind.Snippet, "List (one or more)", "$1+"),
        createCompletion("text{n}", CompletionItemKind.Snippet, "List (exactly n)", "$1{$2}"),
        createCompletion("text{min,max}", CompletionItemKind.Snippet, "List (range)", "$1{$2,$3}")
    )

    private def objectFieldCompletions: List[CompletionItem] = List(
        createCompletion("field:", CompletionItemKind.Property, "Required field", "$1: $2"),
        createCompletion("field?:", CompletionItemKind.Property, "Optional field", "$1?: $2"),
        createCompletion("*", CompletionItemKind.Keyword, "Unconstrained keys", "*: $1")
    )

    private def constraintCompletions: List[CompletionItem] = List(
        // Text constraints
        createCompletion("regex", CompletionItemKind.Property, "Regular expression pattern", "regex = \"$1\""),
        createCompletion("pattern", CompletionItemKind.Property, "Format pattern", "pattern = \"$1\""),
        createCompletion("format", CompletionItemKind.Property, "Format pattern (alias)", "format = \"$1\""),
        createCompletion("length", CompletionItemKind.Property, "Length constraint", "length $1 $2"),

        // Number constraints
        createCompletion("value", CompletionItemKind.Property, "Value constraint", "value $1 $2"),
        createCompletion("integer", CompletionItemKind.Keyword, "Integer constraint", "integer"),

        // List constraints
        createCompletion("unique", CompletionItemKind.Property, "Unique items constraint", "unique"),
        createCompletion("unique = [field]", CompletionItemKind.Snippet, "Unique by fields", "unique = [$1]"),

        // Operators
        createCompletion("==", CompletionItemKind.Operator, "Equal to", "== $1"),
        createCompletion("!=", CompletionItemKind.Operator, "Not equal to", "!= $1"),
        createCompletion("<", CompletionItemKind.Operator, "Less than", "< $1"),
        createCompletion("<=", CompletionItemKind.Operator, "Less than or equal", "<= $1"),
        createCompletion(">", CompletionItemKind.Operator, "Greater than", "> $1"),
        createCompletion(">=", CompletionItemKind.Operator, "Greater than or equal", ">= $1")
    )

    private def allCompletions: List[CompletionItem] = typeCompletions ++ structureCompletions ++ objectFieldCompletions ++ constraintCompletions

    private def createCompletion(
        label: String,
        kind: CompletionItemKind,
        documentation: String,
        insertText: String
    ): CompletionItem =
        val item = CompletionItem(label)
        item.setKind(kind)
        item.setDocumentation(documentation)
        item.setInsertText(insertText)
        item.setInsertTextFormat(InsertTextFormat.Snippet)
        item

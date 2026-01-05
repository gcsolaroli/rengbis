package rengbis.lsp

import org.eclipse.lsp4j.{ Location, Position, Range }
import rengbis.Schema

class DefinitionProvider:

    def provideDefinition(uri: String, text: String, position: Position): Option[Location] =
        val line      = position.getLine()
        val character = position.getCharacter()

        val lines = text.split("\n")
        if line >= 0 && line < lines.length then
            val currentLine = lines(line)
            val word        = getWordAtPosition(currentLine, character)

            word.flatMap(w => findDefinition(uri, text, w))
        else None

    private def getWordAtPosition(line: String, character: Int): Option[String] =
        if character < 0 || character > line.length then None
        else
            val start = (character to 0 by -1)
                .find(i => i == 0 || !isWordChar(line(i - 1)))
                .getOrElse(0)

            val end = (character until line.length)
                .find(i => !isWordChar(line(i)))
                .getOrElse(line.length)

            if start < end then Some(line.substring(start, end))
            else None

    private def isWordChar(c: Char): Boolean =
        c.isLetterOrDigit || c == '_' || c == '-' || c == '.'

    private def findDefinition(uri: String, text: String, identifier: String): Option[Location] =
        // Look for definition patterns:
        // 1. Named definitions: define foo = ...
        // 2. Import statements: import "file.rng" as namespace

        val lines = text.split("\n")

        // Search for "define <identifier> ="       =>  WTF!!! 🤔    Identifiers are defined as `<identifier name> = …`
        val definePattern = s"define\\s+${ java.util.regex.Pattern.quote(identifier) }\\s*=".r

        val defineLocation = lines.zipWithIndex.collectFirst:
            case (line, lineIndex) if definePattern.findFirstMatchIn(line).isDefined =>
                val m     = definePattern.findFirstMatchIn(line).get
                val range = new Range(
                    new Position(lineIndex, m.start),
                    new Position(lineIndex, m.end)
                )
                new Location(uri, range)

        if defineLocation.isDefined then defineLocation
        else
            // If it looks like a namespace reference (e.g., "ns.Type"), try to find import
            if identifier.contains(".") then
                val parts = identifier.split("\\.")
                if parts.length == 2 then
                    val namespace = parts(0)
                    findImportDefinition(uri, text, namespace)
                else None
            else None

    private def findImportDefinition(uri: String, text: String, namespace: String): Option[Location] =
        val lines = text.split("\n")

        // Search for: import "..." as <namespace>          =>  WTF!!! 🤔    Import are done using the `<namespace> => import <path>` syntax.
        val importPattern = s"import\\s+\"[^\"]+\"\\s+as\\s+${ java.util.regex.Pattern.quote(namespace) }".r

        lines.zipWithIndex.collectFirst:
            case (line, lineIndex) if importPattern.findFirstMatchIn(line).isDefined =>
                val m     = importPattern.findFirstMatchIn(line).get
                val range = new Range(
                    new Position(lineIndex, m.start),
                    new Position(lineIndex, m.end)
                )
                new Location(uri, range)

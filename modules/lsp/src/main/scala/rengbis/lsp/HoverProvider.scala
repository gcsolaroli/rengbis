package rengbis.lsp

import org.eclipse.lsp4j.{ Hover, MarkupContent, MarkupKind, Position }
import rengbis.Schema

class HoverProvider:

    def provideHover(text: String, position: Position): Option[Hover] =
        wordAtCursorPosition(text, position).flatMap(w => getHoverInfo(w))

    private def wordAtCursorPosition(text: String, position: Position): Option[String] =
        val line      = position.getLine()
        val character = position.getCharacter()

        val lines = text.split("\n")
        if line >= 0 && line < lines.length then
            val currentLine = lines(line)
            getWordAtPosition(currentLine, character)
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

            if start < end
            then Some(line.substring(start, end))
            else None

    private def isWordChar(c: Char): Boolean = c.isLetterOrDigit || c == '_' || c == '-'

    private def getHoverInfo(word: String): Option[Hover] =
        word match
            case "text" =>
                Some(
                    createHover(
                        "**text**",
                        "Matches string values",
                        "Example: `= text`",
                        "With constraints: `= text { regex = \"[a-z]+\", length == 10 }`"
                    )
                )

            case "number" =>
                Some(
                    createHover(
                        "**number**",
                        "Matches numeric values (integers and decimals)",
                        "Example: `= number`",
                        "With constraints: `= number { 0 < value <= 100, integer }`"
                    )
                )

            case "boolean" =>
                Some(
                    createHover(
                        "**boolean**",
                        "Matches boolean values (true/false)",
                        "Example: `= boolean`"
                    )
                )

            case "any" =>
                Some(
                    createHover(
                        "**any**",
                        "Matches any value type",
                        "Example: `= any`"
                    )
                )

            case "regex" =>
                Some(
                    createHover(
                        "**regex** constraint",
                        "Specifies a regular expression pattern for text values",
                        "Example: `text { regex = \"^[A-Z][a-z]+$\" }`"
                    )
                )

            case "pattern" | "format" =>
                Some(
                    createHover(
                        "**pattern/format** constraint",
                        "Specifies a format pattern for text values",
                        "Format characters:",
                        "- `#` = digit (0-9)",
                        "- `X` = letter (a-z, A-Z)",
                        "- `@` = alphanumeric",
                        "- `*` = any character",
                        "Example: `text { pattern = \"(###) ###-####\" }`"
                    )
                )

            case "length" =>
                Some(
                    createHover(
                        "**length** constraint",
                        "Specifies length constraints for text values",
                        "Example: `text { length == 10 }`",
                        "Range: `text { 5 < length <= 20 }`"
                    )
                )

            case "value" =>
                Some(
                    createHover(
                        "**value** constraint",
                        "Specifies numeric range constraints",
                        "Example: `number { value >= 0 }`",
                        "Range: `number { 0 <= value < 100 }`"
                    )
                )

            case "integer" =>
                Some(
                    createHover(
                        "**integer** constraint",
                        "Requires number to be an integer (no decimal part)",
                        "Example: `number { integer }`"
                    )
                )

            case "unique" =>
                Some(
                    createHover(
                        "**unique** constraint",
                        "Requires all items in a list to be unique",
                        "Simple: `text* { unique }`",
                        "By fields: `{ name: text, age: number }* { unique = [name] }`"
                    )
                )

            case _ =>
                None

    private def createHover(parts: String*): Hover =
        val content = new MarkupContent()
        content.setKind(MarkupKind.MARKDOWN)
        content.setValue(parts.mkString("\n\n"))

        Hover(content)

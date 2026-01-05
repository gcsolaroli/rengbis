package rengbis.lsp

import org.eclipse.lsp4j.{ Diagnostic, DiagnosticSeverity, Position, Range }
import rengbis.Schema

object DiagnosticsProvider:

    def provideDiagnostics(uri: String, text: String): List[Diagnostic] =
        Schema.parse(text) match
            case Left(error) => List(createDiagnostic(error))
            case Right(_)    => List.empty

    private def createDiagnostic(errorMessage: String): Diagnostic =
        val (line, character, message) = parseErrorMessage(errorMessage)

        val range = new Range(
            new Position(line, character),
            new Position(line, character + 1)
        )

        val diagnostic = Diagnostic(range, message)
        diagnostic.setSeverity(DiagnosticSeverity.Error)
        diagnostic.setSource("rengbis")
        diagnostic

    private def parseErrorMessage(error: String): (Int, Int, String) =
        val linePattern   = """.*line\s+(\d+).*""".r
        val columnPattern = """.*column\s+(\d+).*""".r

        val line = error match
            case linePattern(l) => l.toInt - 1 // LSP uses 0-based indexing
            case _              => 0

        val column = error match
            case columnPattern(c) => c.toInt - 1 // LSP uses 0-based indexing
            case _                => 0

        (line, column, error)

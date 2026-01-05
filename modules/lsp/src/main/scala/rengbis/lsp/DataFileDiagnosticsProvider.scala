package rengbis.lsp

import java.net.URI
import java.nio.file.Path
import org.eclipse.lsp4j.{ Diagnostic, DiagnosticSeverity, Position, Range }
import rengbis.{ DataParsers, Validator }
import rengbis.Validator.ValidationError
import zio.Chunk

object DataFileDiagnosticsProvider:

    def provideDiagnostics(
        uri: String,
        text: String,
        fileType: DataFileType,
        schemaCache: SchemaCache
    ): List[Diagnostic] =
        val schemaPathOpt = SchemaDirectiveExtractor.extract(text, fileType)
        schemaPathOpt match
            case None               => List.empty
            case Some(relativePath) =>
                val dataFilePath = Path.of(URI.create(uri))
                val schemaPath   = dataFilePath.getParent.resolve(relativePath).normalize()

                schemaCache.getOrLoad(schemaPath) match
                    case Left(error)     => List(createDiagnostic(0, 0, s"Failed to load schema: $error"))
                    case Right(resolved) =>
                        val parseResult = fileType match
                            case DataFileType.Json => DataParsers.json(text)
                            case DataFileType.Yaml => DataParsers.yaml(text)
                            case DataFileType.Xml  => DataParsers.xml(text)

                        parseResult match
                            case Left(parseError) => List(createDiagnostic(0, 0, s"Parse error: $parseError"))
                            case Right(value)     =>
                                val result = Validator.validateValue(resolved.rootSchema, value)
                                if result.isValid
                                then List.empty
                                else
                                    result.value match
                                        case errors: Chunk[ValidationError @unchecked] =>
                                            errors.map(e => createDiagnostic(0, 0, e.message)).toList
                                        case _                                         => List.empty

    private def createDiagnostic(line: Int, character: Int, message: String): Diagnostic =
        val range = new Range(
            new Position(line, character),
            new Position(line, character + 1)
        )

        val diagnostic = Diagnostic(range, message)
        diagnostic.setSeverity(DiagnosticSeverity.Error)
        diagnostic.setSource("rengbis")
        diagnostic

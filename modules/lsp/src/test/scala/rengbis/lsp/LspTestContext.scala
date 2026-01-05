package rengbis.lsp

import org.eclipse.lsp4j.{ CompletionItem, CompletionParams, DefinitionParams, Diagnostic, InitializeParams, InitializedParams, Location, Position, Range }
import org.eclipse.lsp4j.{ MessageActionItem, MessageParams, PublishDiagnosticsParams, ShowMessageRequestParams }
import org.eclipse.lsp4j.{ TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, VersionedTextDocumentIdentifier }
import org.eclipse.lsp4j.{ DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams }
import org.eclipse.lsp4j.{ Hover, HoverParams }
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.eclipse.lsp4j.services.LanguageClient
import scala.collection.mutable
import scala.jdk.CollectionConverters.{ ListHasAsScala, SeqHasAsJava }
import java.util.concurrent.CompletableFuture

class LspTestContext:
    private val server = RengbisLanguageServer()
    private val client = TestLanguageClient()

    private val diagnosticsMap = mutable.Map[String, List[Diagnostic]]()

    private var currentUri: String               = ""
    private var currentText: String              = ""
    private var currentVersion: Int              = 0
    private var currentPosition: Position | Null = null
    private var currentRange: Range | Null       = null

    server.connect(client)
    server.initialize(InitializeParams()).get()
    server.initialized(InitializedParams())

    def withDocument(uri: String, text: String, version: Int = 1): LspTestContext =
        currentUri = uri
        currentText = text
        currentVersion = version

        val textDocument = TextDocumentItem(uri, "rengbis", version, text)
        val params       = DidOpenTextDocumentParams(textDocument)

        server.getTextDocumentService().didOpen(params)
        this.giveDiagnosticsAMomentToBePublished()

    def updateDocument(text: String): LspTestContext =
        currentVersion += 1
        currentText = text

        val textDocument = VersionedTextDocumentIdentifier(currentUri, currentVersion)
        val change       = TextDocumentContentChangeEvent(text)
        val params       = DidChangeTextDocumentParams(textDocument, List(change).asJava)

        server.getTextDocumentService().didChange(params)
        this.giveDiagnosticsAMomentToBePublished()

    def saveDocument(): LspTestContext =
        val textDocument = TextDocumentIdentifier(currentUri)
        val params       = DidSaveTextDocumentParams(textDocument)

        server.getTextDocumentService().didSave(params)
        this

    def closeDocument(): LspTestContext =
        val textDocument = TextDocumentIdentifier(currentUri)
        val params       = DidCloseTextDocumentParams(textDocument)

        server.getTextDocumentService().didClose(params)
        this.giveDiagnosticsAMomentToBePublished()

    def atPosition(line: Int, character: Int): LspTestContext =
        currentPosition = Position(line, character)
        this

    def withRange(startLine: Int, startChar: Int, endLine: Int, endChar: Int): LspTestContext =
        currentRange = Range(Position(startLine, startChar), Position(endLine, endChar))
        this

    def completion(): List[CompletionItem] =
        require(currentPosition != null, "Position must be set with atPosition() before calling completion()")

        val textDocument = TextDocumentIdentifier(currentUri)
        val params       = CompletionParams(textDocument, currentPosition)

        val result = server.getTextDocumentService().completion(params).get()

        result.getLeft() match
            case null  => List.empty
            case items => items.asScala.toList

    def hover(): Option[Hover] =
        require(currentPosition != null, "Position must be set with atPosition() before calling hover()")

        val textDocument = TextDocumentIdentifier(currentUri)
        val params       = HoverParams(textDocument, currentPosition)

        Option(server.getTextDocumentService().hover(params).get())

    def definition(): List[Location] =
        require(currentPosition != null, "Position must be set with atPosition() before calling definition()")

        val textDocument = TextDocumentIdentifier(currentUri)
        val params       = DefinitionParams(textDocument, currentPosition)

        val result = server.getTextDocumentService().definition(params).get()

        result.getLeft() match
            case null      => List.empty
            case locations => locations.asScala.toList.map(_.asInstanceOf[Location])

    def currentDiagnostics: List[Diagnostic]          = diagnosticsMap.getOrElse(currentUri, List.empty)
    def diagnosticsFor(uri: String): List[Diagnostic] = diagnosticsMap.getOrElse(uri, List.empty)
    def clearDiagnostics(): Unit                      = diagnosticsMap.clear()

    def uri: String  = currentUri
    def text: String = currentText
    def version: Int = currentVersion

    def position: Option[Position] = Option(currentPosition.asInstanceOf[Position])
    def range: Option[Range]       = Option(currentRange.asInstanceOf[Range])

    private def giveDiagnosticsAMomentToBePublished(): LspTestContext = { Thread.sleep(10); this }

    // ========================================================================

    private class TestLanguageClient extends LanguageClient:
        override def publishDiagnostics(params: PublishDiagnosticsParams): Unit = diagnosticsMap(params.getUri()) = params.getDiagnostics().asScala.toList

        override def telemetryEvent(obj: Object): Unit                                                          = ()
        override def logMessage(params: MessageParams): Unit                                                    = ()
        override def showMessage(params: MessageParams): Unit                                                   = ()
        override def showMessageRequest(params: ShowMessageRequestParams): CompletableFuture[MessageActionItem] = CompletableFuture.completedFuture(null)

object LspTestContext: //   WTF! 🤔      These methods, especially `pos` and `range` look really silly and un-useful
    def apply(): LspTestContext                                                  = new LspTestContext()
    def pos(line: Int, char: Int): Position                                      = Position(line, char)
    def range(startLine: Int, startChar: Int, endLine: Int, endChar: Int): Range = Range(Position(startLine, startChar), Position(endLine, endChar))

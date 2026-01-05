package rengbis.lsp

import org.eclipse.lsp4j.{ CompletionItem, CompletionList, CompletionParams, PublishDiagnosticsParams }
import org.eclipse.lsp4j.{ DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams }
import org.eclipse.lsp4j.{ Hover, HoverParams }
import org.eclipse.lsp4j.{ DefinitionParams, Location, LocationLink }
import org.eclipse.lsp4j.{ DocumentSymbol, DocumentSymbolParams, SymbolInformation }
import org.eclipse.lsp4j.services.{ LanguageClient, TextDocumentService }
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.SeqHasAsJava

class RengbisTextDocumentService extends TextDocumentService:
    private var client: LanguageClient | Null = null
    private val documentManager               = DocumentManager()
    private val schemaCache                   = SchemaCache()
    private val completionProvider            = CompletionProvider()
    private val hoverProvider                 = HoverProvider()
    private val definitionProvider            = DefinitionProvider()

    def connect(client: LanguageClient): Unit = this.client = client

    override def didOpen(params: DidOpenTextDocumentParams): Unit =
        val doc = params.getTextDocument()
        documentManager.openDocument(doc.getUri(), doc.getText(), doc.getVersion())
        publishDiagnostics(doc.getUri(), doc.getText())

    override def didChange(params: DidChangeTextDocumentParams): Unit =
        val doc     = params.getTextDocument()
        val changes = params.getContentChanges()

        if !changes.isEmpty then
            // Full text sync - take the last change which contains full document
            val newText = changes.get(changes.size() - 1).getText()
            documentManager.updateDocument(doc.getUri(), newText, doc.getVersion())
            publishDiagnostics(doc.getUri(), newText)

    override def didClose(params: DidCloseTextDocumentParams): Unit =
        val uri = params.getTextDocument().getUri()
        documentManager.closeDocument(uri)
        client match
            case c: LanguageClient =>
                c.publishDiagnostics(PublishDiagnosticsParams(uri, List.empty.asJava))
            case null              => ()

    override def didSave(params: DidSaveTextDocumentParams): Unit = () // Optional: could trigger additional validation on save

    override def completion(params: CompletionParams): CompletableFuture[JEither[java.util.List[CompletionItem], CompletionList]] =
        val uri      = params.getTextDocument().getUri()
        val position = params.getPosition()

        if uri.endsWith(".rengbis")
        then
            documentManager.getDocument(uri) match
                case Some(doc) =>
                    val items = completionProvider.provideCompletions(doc.text, position)
                    CompletableFuture.completedFuture(JEither.forLeft(items.asJava))
                case None      =>
                    CompletableFuture.completedFuture(JEither.forLeft(List.empty.asJava))
        else CompletableFuture.completedFuture(JEither.forLeft(List.empty.asJava))

    override def hover(params: HoverParams): CompletableFuture[Hover] =
        val uri      = params.getTextDocument().getUri()
        val position = params.getPosition()

        if uri.endsWith(".rengbis")
        then
            documentManager.getDocument(uri) match
                case Some(doc) =>
                    hoverProvider.provideHover(doc.text, position) match
                        case Some(hover) => CompletableFuture.completedFuture(hover)
                        case None        => CompletableFuture.completedFuture(null)
                case None      =>
                    CompletableFuture.completedFuture(null)
        else CompletableFuture.completedFuture(null)

    override def definition(params: DefinitionParams): CompletableFuture[JEither[java.util.List[? <: Location], java.util.List[? <: LocationLink]]] =
        val uri      = params.getTextDocument().getUri()
        val position = params.getPosition()

        if uri.endsWith(".rengbis")
        then
            documentManager.getDocument(uri) match
                case Some(doc) =>
                    definitionProvider.provideDefinition(uri, doc.text, position) match
                        case Some(location) =>
                            CompletableFuture.completedFuture(JEither.forLeft(List(location).asJava))
                        case None           =>
                            CompletableFuture.completedFuture(JEither.forLeft(List.empty.asJava))
                case None      =>
                    CompletableFuture.completedFuture(JEither.forLeft(List.empty.asJava))
        else CompletableFuture.completedFuture(JEither.forLeft(List.empty.asJava))

    override def documentSymbol(params: DocumentSymbolParams): CompletableFuture[java.util.List[JEither[SymbolInformation, DocumentSymbol]]] =
        // TODO: Implement document symbols for outline view
        CompletableFuture.completedFuture(List.empty.asJava)

    private def publishDiagnostics(uri: String, text: String): Unit =
        client match
            case c: LanguageClient =>
                val diagnostics = DataFileType.fromUri(uri) match
                    case Some(fileType)                   => DataFileDiagnosticsProvider.provideDiagnostics(uri, text, fileType, schemaCache)
                    case None if uri.endsWith(".rengbis") => DiagnosticsProvider.provideDiagnostics(uri, text)
                    case None                             => List.empty
                c.publishDiagnostics(PublishDiagnosticsParams(uri, diagnostics.asJava))
            case null              => ()

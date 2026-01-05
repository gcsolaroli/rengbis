package rengbis.lsp

import org.eclipse.lsp4j.{ CompletionOptions, InitializeParams, InitializeResult, InitializedParams, ServerCapabilities, TextDocumentSyncKind }
import org.eclipse.lsp4j.services.{ LanguageClient, LanguageServer, TextDocumentService, WorkspaceService }
import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters.SeqHasAsJava

class RengbisLanguageServer extends LanguageServer:
    private var client: LanguageClient | Null = null
    private val textDocumentService           = RengbisTextDocumentService()
    private val workspaceService              = RengbisWorkspaceService()

    def connect(client: LanguageClient): Unit =
        this.client = client
        textDocumentService.connect(client)

    override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
        val capabilities = ServerCapabilities()

        val completionOptions = CompletionOptions()
        completionOptions.setTriggerCharacters(List(":", "{", "|", "=").asJava)
        capabilities.setCompletionProvider(completionOptions)

        capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
        capabilities.setHoverProvider(true)
        capabilities.setDefinitionProvider(true)
        capabilities.setDocumentSymbolProvider(true)

        val result = InitializeResult(capabilities)
        CompletableFuture.completedFuture(result)

    override def initialized(params: InitializedParams): Unit  = ()
    override def shutdown(): CompletableFuture[Object]         = CompletableFuture.completedFuture(new Object())
    override def exit(): Unit                                  = System.exit(0)
    override def getTextDocumentService(): TextDocumentService = textDocumentService
    override def getWorkspaceService(): WorkspaceService       = workspaceService

package rengbis.lsp

import org.eclipse.lsp4j.services.WorkspaceService
import org.eclipse.lsp4j.{ DidChangeConfigurationParams, DidChangeWatchedFilesParams }
import java.util.concurrent.CompletableFuture

class RengbisWorkspaceService extends WorkspaceService:
    override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = () // Handle configuration changes if needed
    override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit   = () // Handle file system changes if needed

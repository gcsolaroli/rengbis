package rengbis.lsp

import org.eclipse.lsp4j.launch.LSPLauncher
import java.io.InputStream
import java.io.OutputStream
import java.util.concurrent.Executors
import java.util.logging.LogManager
import java.util.logging.Logger

object Main:
    def main(args: Array[String]): Unit =
        LogManager.getLogManager().reset() // Disable default logging to avoid interference with LSP communication
        val server   = RengbisLanguageServer()
        val launcher = LSPLauncher.createServerLauncher(
            server,
            System.in,
            System.out
        )

        val client = launcher.getRemoteProxy()
        server.connect(client)
        launcher.startListening()

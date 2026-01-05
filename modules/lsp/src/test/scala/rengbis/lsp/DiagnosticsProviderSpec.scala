package rengbis.lsp

import zio.test.{ assertTrue, suite, test, TestAspect, ZIOSpecDefault }
import org.eclipse.lsp4j.DiagnosticSeverity

object DiagnosticsProviderSpec extends ZIOSpecDefault:

    def spec = suite("DiagnosticsProvider")(
        test("should report no diagnostics for valid schema") {
            val ctx = LspTestContext()
                .withDocument(
                    "file:///test.rengbis",
                    """person = { name: text, age: number }
                      |= person""".stripMargin
                )

            val diagnostics = ctx.currentDiagnostics

            assertTrue(diagnostics.isEmpty)
        },
        test("should report error for invalid syntax") {
            val ctx = LspTestContext()
                .withDocument(
                    "file:///test.rengbis",
                    "= { invalid syntax here"
                )

            val diagnostics = ctx.currentDiagnostics

            assertTrue(
                diagnostics.nonEmpty,
                diagnostics.head.getSeverity() == DiagnosticSeverity.Error,
                diagnostics.head.getSource() == "rengbis"
            )
        },
        test("should update diagnostics when document changes") {
            val ctx = LspTestContext()
                .withDocument("file:///test.rengbis", "= text")

            assertTrue(ctx.currentDiagnostics.isEmpty)

            ctx.updateDocument("= { broken")

            assertTrue(ctx.currentDiagnostics.nonEmpty)

            ctx.updateDocument("= text")

            assertTrue(ctx.currentDiagnostics.isEmpty)
        },
        test("should clear diagnostics when document is closed") {
            val ctx = LspTestContext()
                .withDocument("file:///test.rengbis", "= { broken")

            assertTrue(ctx.currentDiagnostics.nonEmpty)

            ctx.closeDocument()

            assertTrue(ctx.currentDiagnostics.isEmpty)
        },
        test("should report diagnostics with line and column information") {
            val ctx = LspTestContext()
                .withDocument(
                    "file:///test.rengbis",
                    """person = { name: text }
                      |= invalid here""".stripMargin
                )

            val diagnostics = ctx.currentDiagnostics

            // Should have position information
            assertTrue(
                diagnostics.nonEmpty,
                diagnostics.head.getRange() != null,
                diagnostics.head.getRange().getStart() != null
            )
        },
        test("should handle multiple documents independently") {
            val ctx1 = LspTestContext()
                .withDocument("file:///valid.rengbis", "= text")

            val ctx2 = LspTestContext()
                .withDocument("file:///invalid.rengbis", "= { broken")

            assertTrue(
                ctx1.currentDiagnostics.isEmpty,
                ctx2.currentDiagnostics.nonEmpty
            )
        }
    )

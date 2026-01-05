package rengbis.lsp

import zio.test.{ assertTrue, suite, test, TestAspect, ZIOSpecDefault }
import org.eclipse.lsp4j.MarkupKind

object HoverProviderSpec extends ZIOSpecDefault:

    def spec = suite("HoverProvider")(
        suite("Type keyword hover")(
            test("should provide hover info for 'text' keyword") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text")
                    .atPosition(0, 3) // cursor on "text"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("text"),
                    hover.get.getContents().getRight().getValue().contains("string")
                )
            },
            test("should provide hover info for 'number' keyword") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= number")
                    .atPosition(0, 4) // cursor on "number"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("number"),
                    hover.get.getContents().getRight().getValue().contains("numeric")
                )
            },
            test("should provide hover info for 'boolean' keyword") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= boolean")
                    .atPosition(0, 5) // cursor on "boolean"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("boolean"),
                    hover.get.getContents().getRight().getValue().toLowerCase().contains("true/false")
                )
            },
            test("should provide hover info for 'any' keyword") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= any")
                    .atPosition(0, 3) // cursor on "any"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("any")
                )
            }
        ),
        suite("Constraint keyword hover")(
            test("should provide hover info for 'regex' constraint") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text [ regex = \".*\" ]")
                    .atPosition(0, 10) // cursor on "regex"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("regex"),
                    hover.get.getContents().getRight().getValue().contains("pattern")
                )
            },
            test("should provide hover info for 'pattern' constraint") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text [ pattern = \"###\" ]")
                    .atPosition(0, 12) // cursor on "pattern"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("pattern"),
                    hover.get.getContents().getRight().getValue().contains("#")
                )
            },
            test("should provide hover info for 'length' constraint") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text [ length == 10 ]")
                    .atPosition(0, 11) // cursor on "length"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("length")
                )
            },
            test("should provide hover info for 'value' constraint") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= number [ value >= 0 ]")
                    .atPosition(0, 13) // cursor on "value"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("value"),
                    hover.get.getContents().getRight().getValue().contains("numeric")
                )
            },
            test("should provide hover info for 'integer' constraint") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= number [ integer ]")
                    .atPosition(0, 13) // cursor on "integer"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("integer")
                )
            },
            test("should provide hover info for 'unique' constraint") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text* [ unique ]")
                    .atPosition(0, 12) // cursor on "unique"

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("unique")
                )
            }
        ),
        suite("Hover content format")(
            test("should use markdown format") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text")
                    .atPosition(0, 3)

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getKind() == MarkupKind.MARKDOWN
                )
            },
            test("should include examples in hover content") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text")
                    .atPosition(0, 3)

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("Example")
                )
            }
        ),
        suite("Hover position handling")(
            test("should return None for non-keyword positions") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text")
                    .atPosition(0, 0) // cursor on "="

                val hover = ctx.hover()
                assertTrue(hover.isEmpty)
            },
            test("should handle multi-line documents") {
                val ctx = LspTestContext()
                    .withDocument(
                        "file:///test.rengbis",
                        """person = { name: text }
                          |= number""".stripMargin
                    )
                    .atPosition(1, 3) // cursor on "number" on second line

                val hover = ctx.hover()
                assertTrue(
                    hover.isDefined,
                    hover.get.getContents().getRight().getValue().contains("number")
                )
            },
            test("should handle cursor in middle of word") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= boolean")
                    .atPosition(0, 5) // cursor in middle of "boolean"

                val hover = ctx.hover()
                assertTrue(hover.isDefined)
            }
        ),
        suite("No hover for unknown tokens")(
            test("should return None for unknown identifiers") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "person = text")
                    .atPosition(0, 2) // cursor on "person"

                val hover = ctx.hover()
                assertTrue(hover.isEmpty)
            },
            test("should return None for whitespace") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "=   text")
                    .atPosition(0, 2) // cursor on whitespace

                val hover = ctx.hover()
                assertTrue(hover.isEmpty)
            }
        )
    )

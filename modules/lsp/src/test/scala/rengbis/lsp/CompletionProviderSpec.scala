package rengbis.lsp

import zio.test.{ assertTrue, suite, test, TestAspect, ZIOSpecDefault }
import org.eclipse.lsp4j.CompletionItemKind

object CompletionProviderSpec extends ZIOSpecDefault:

    def spec = suite("CompletionProvider")(
        suite("Type completions")(
            test("should suggest types after equals sign") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= ")
                    .atPosition(0, 2)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(
                    labels.contains("text"),
                    labels.contains("number"),
                    labels.contains("boolean"),
                    labels.contains("any")
                )
            },
            test("should suggest types at beginning of line") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "")
                    .atPosition(0, 0)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(labels.contains("text"))
            },
            test("should suggest types after colon in object") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= { name: }")
                    .atPosition(0, 10)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(
                    labels.contains("text"),
                    labels.contains("number")
                )
            }
        ),
        suite("Structure completions")(
            test("should suggest object and list structures") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= ")
                    .atPosition(0, 2)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(
                    labels.contains("{ }"),
                    labels.contains("[ ]"),
                    labels.contains("text*"),
                    labels.contains("text+")
                )
            },
            test("should provide insert text with placeholders") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= ")
                    .atPosition(0, 2)

                val completions      = ctx.completion()
                val objectCompletion = completions.find(_.getLabel() == "{ }")

                assertTrue(
                    objectCompletion.isDefined,
                    objectCompletion.get.getInsertText() == "{ $1: $2 }"
                )
            }
        ),
        suite("Object field completions")(
            test("should suggest field syntax inside object") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= { ")
                    .atPosition(0, 4)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(
                    labels.contains("field:"),
                    labels.contains("field?:"),
                    labels.contains("*")
                )
            },
            test("should suggest unconstrained keys in object") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= { name: text, ")
                    .atPosition(0, 16)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(labels.contains("*"))
            } @@ TestAspect.ignore
        ),
        suite("Constraint completions")(
            test("should suggest text constraints") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text { ")
                    .atPosition(0, 9)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(
                    labels.contains("regex"),
                    labels.contains("pattern"),
                    labels.contains("length")
                )
            },
            test("should suggest number constraints") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= number { ")
                    .atPosition(0, 11)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(
                    labels.contains("value"),
                    labels.contains("integer")
                )
            },
            test("should suggest comparison operators") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= number { ")
                    .atPosition(0, 11)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(
                    labels.contains("=="),
                    labels.contains("!="),
                    labels.contains("<"),
                    labels.contains("<="),
                    labels.contains(">"),
                    labels.contains(">=")
                )
            },
            test("should suggest list constraints") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text* { ")
                    .atPosition(0, 10)

                val completions = ctx.completion()
                val labels      = completions.map(_.getLabel())

                assertTrue(labels.contains("unique"))
            }
        ),
        suite("Completion item details")(
            test("should include documentation") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= ")
                    .atPosition(0, 2)

                val completions    = ctx.completion()
                val textCompletion = completions.find(_.getLabel() == "text")

                assertTrue(
                    textCompletion.isDefined,
                    textCompletion.get.getDocumentation() != null
                )
            },
            test("should set appropriate completion item kinds") {
                val ctx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= ")
                    .atPosition(0, 2)

                val completions      = ctx.completion()
                val textCompletion   = completions.find(_.getLabel() == "text")
                val objectCompletion = completions.find(_.getLabel() == "{ }")

                assertTrue(
                    textCompletion.get.getKind() == CompletionItemKind.Keyword,
                    objectCompletion.get.getKind() == CompletionItemKind.Snippet
                )
            }
        ),
        suite("Context-aware completions")(
            test("should provide different completions in different contexts") {
                val typeCtx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= ")
                    .atPosition(0, 2)

                val constraintCtx = LspTestContext()
                    .withDocument("file:///test.rengbis", "= text { ")
                    .atPosition(0, 9)

                val typeLabels       = typeCtx.completion().map(_.getLabel()).toSet
                val constraintLabels = constraintCtx.completion().map(_.getLabel()).toSet

                assertTrue(
                    typeLabels.contains("text"),
                    constraintLabels.contains("regex"),
                    typeLabels != constraintLabels
                )
            }
        )
    )

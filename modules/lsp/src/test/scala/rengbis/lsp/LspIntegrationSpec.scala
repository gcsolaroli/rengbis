package rengbis.lsp

import zio.test.{ assertTrue, suite, test, TestAspect, ZIOSpecDefault }

object LspIntegrationSpec extends ZIOSpecDefault:

    def spec = suite("LSP Integration Tests")(
        test("workflow: open document, get diagnostics, fix error, verify clean") {
            val ctx = LspTestContext()
            ctx.withDocument("file:///workflow.rengbis", "= { broken syntax")
            assertTrue(ctx.currentDiagnostics.nonEmpty)
            ctx.updateDocument("= { name: text }")
            assertTrue(ctx.currentDiagnostics.isEmpty)
        },
        test("workflow: type code with completions") {
            val ctx         = LspTestContext()
            ctx.withDocument("file:///typing.rengbis", "")
            ctx.updateDocument("= ").atPosition(0, 2)
            val completions = ctx.completion()
            val labels      = completions.map(_.getLabel())
            assertTrue(
                labels.contains("text"),
                labels.contains("{ }"),
                labels.contains("text*")
            )

            ctx.updateDocument("= text [").atPosition(0, 8)
            val constraintCompletions = ctx.completion()
            val constraintLabels      = constraintCompletions.map(_.getLabel())
            assertTrue(
                constraintLabels.contains("regex"),
                constraintLabels.contains("pattern")
            )
        },
        test("workflow: hover while reading schema") {
            val ctx = LspTestContext().withDocument(
                "file:///learning.rengbis",
                """person = { name: text, age: number [ integer ] }
                      |= person*""".stripMargin
            )

            val textHover = ctx.atPosition(0, 22).hover()
            assertTrue(
                textHover.isDefined,
                textHover.get.getContents().getRight().getValue().contains("string")
            )

            val integerHover = ctx.atPosition(0, 40).hover()
            assertTrue(
                integerHover.isDefined,
                integerHover.get.getContents().getRight().getValue().contains("integer")
            )

            val numberHover = ctx.atPosition(0, 32).hover()
            assertTrue(numberHover.isDefined)
        },
        test("workflow: navigate to definition") {
            val ctx = LspTestContext().withDocument(
                "file:///navigation.rengbis",
                """user_id = text [ pattern = \"U####\" ]
                      |email = text [ regex = \".*@.*\" ]
                      |user = { id: user_id, email: email }
                      |= user*""".stripMargin
            )

            val userIdDef = ctx.atPosition(2, 15).definition()
            assertTrue(
                userIdDef.nonEmpty,
                userIdDef.head.getRange().getStart().getLine() == 0
            )

            val emailDef = ctx.atPosition(2, 29).definition()
            assertTrue(
                emailDef.nonEmpty,
                emailDef.head.getRange().getStart().getLine() == 1
            )

            val userDef = ctx.atPosition(3, 3).definition()
            assertTrue(
                userDef.nonEmpty,
                userDef.head.getRange().getStart().getLine() == 2
            )
        } @@ TestAspect.ignore,
        test("workflow: incremental editing with diagnostics") {
            val ctx = LspTestContext().withDocument("file:///incremental.rengbis", "= text")
            assertTrue(ctx.currentDiagnostics.isEmpty)

            ctx.updateDocument("= text [")
            assertTrue(ctx.currentDiagnostics.nonEmpty)

            ctx.updateDocument("= text [ regex")
            assertTrue(ctx.currentDiagnostics.nonEmpty)

            ctx.updateDocument("= text [ regex = \".*\" ]")
            assertTrue(ctx.currentDiagnostics.isEmpty)
        },
        test("workflow: multiple documents") {
            val ctx1 = LspTestContext().withDocument("file:///types.rengbis", "person = { name: text }")
            val ctx2 = LspTestContext().withDocument("file:///data.rengbis", "types => import ./types.rengbis")

            assertTrue(
                ctx1.currentDiagnostics.isEmpty,
                ctx2.currentDiagnostics.isEmpty
            )

            val completions1 = ctx1.atPosition(0, 18).completion()
            val completions2 = ctx2.atPosition(0, 10).completion()

            assertTrue(
                completions1.nonEmpty,
                completions2.nonEmpty
            )
        },
        test("workflow: save and close lifecycle") {
            val ctx = LspTestContext().withDocument("file:///lifecycle.rengbis", "= text")
            assertTrue(ctx.currentDiagnostics.isEmpty)

            ctx.saveDocument()
            assertTrue(ctx.currentDiagnostics.isEmpty)

            ctx.updateDocument("= { broken")
            assertTrue(ctx.currentDiagnostics.nonEmpty)

            ctx.closeDocument()
            assertTrue(ctx.currentDiagnostics.isEmpty)
        },
        test("workflow: complex schema development") {
            val ctx = LspTestContext()

            ctx.withDocument(
                "file:///complex.rengbis",
                """email_type = text [ regex = \"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$\" ]
                  |phone_type = text [ pattern = \"(###) ###-####\" ]
                  |""".stripMargin
            )
            assertTrue(ctx.currentDiagnostics.isEmpty)

            ctx.updateDocument(
                """email_type = text [ regex = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ]
                  |phone_type = text [ pattern = "(###) ###-####" ]
                  |age_type = number [ 0 <= value <= 150, integer ]
                  |""".stripMargin
            )
            assertTrue(ctx.currentDiagnostics.isEmpty)

            ctx.updateDocument(
                """email_type = text [ regex = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ]
                  |phone_type = text [ pattern = "(###) ###-####" ]
                  |age_type = number [ 0 <= value <= 150, integer ]
                  |person = {
                  |  name: text,
                  |  email: email_type,
                  |  phone?: phone_type,
                  |  age: age_type
                  |}
                  |= person*
                  |""".stripMargin
            )
            assertTrue(ctx.currentDiagnostics.isEmpty)

            val emailTypeDef = ctx.atPosition(5, 11).definition()
            assertTrue(
                emailTypeDef.nonEmpty,
                emailTypeDef.head.getRange().getStart().getLine() == 0
            )

            val ageHover = ctx.atPosition(2, 23).hover()
            assertTrue(ageHover.isDefined)
        } @@ TestAspect.ignore,
        test("workflow: error recovery suggestions via completion") {
            val ctx = LspTestContext().withDocument("file:///recovery.rengbis", "= { name text }")
            assertTrue(ctx.currentDiagnostics.nonEmpty)

            ctx.atPosition(0, 8)
            val completions = ctx.completion()
            assertTrue(completions.nonEmpty)
        }
    )

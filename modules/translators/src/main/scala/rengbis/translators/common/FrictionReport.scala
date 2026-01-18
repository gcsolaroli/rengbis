package rengbis.translators.common

/** Friction report for tracking translation issues.
  *
  * A friction report captures information about schema features that cannot be perfectly translated to the target format, either due to limitations of the target format or differences in semantics.
  *
  * Friction types:
  *   - Loss: Feature is completely lost in translation (e.g., custom constraints not supported in target)
  *   - Approximation: Feature is approximated or weakened (e.g., stricter constraint becomes looser)
  *   - Extension: Feature requires target-specific extensions (e.g., custom JSON Schema keywords)
  */
enum FrictionType:
    case Loss
    case Approximation
    case Extension

case class FrictionEntry(
    frictionType: FrictionType,
    path: String,
    message: String,
    suggestion: Option[String] = None
)

case class FrictionReport(entries: List[FrictionEntry] = List.empty):
    def addLoss(path: String, message: String, suggestion: Option[String] = None): FrictionReport =
        copy(entries = entries :+ FrictionEntry(FrictionType.Loss, path, message, suggestion))

    def addApproximation(path: String, message: String, suggestion: Option[String] = None): FrictionReport =
        copy(entries = entries :+ FrictionEntry(FrictionType.Approximation, path, message, suggestion))

    def addExtension(path: String, message: String, suggestion: Option[String] = None): FrictionReport =
        copy(entries = entries :+ FrictionEntry(FrictionType.Extension, path, message, suggestion))

    def merge(other: FrictionReport): FrictionReport =
        copy(entries = entries ++ other.entries)

    def isEmpty: Boolean = entries.isEmpty

    def nonEmpty: Boolean = entries.nonEmpty

    /** Renders the friction report as Markdown.
      *
      * @return
      *   A Markdown-formatted string suitable for documentation or CLI output
      */
    def toMarkdown: String =
        if isEmpty then "No translation issues detected."
        else
            val grouped  = entries.groupBy(_.frictionType)
            val sections = List(
                (FrictionType.Loss, "❌ **Loss**", "Features completely lost in translation"),
                (FrictionType.Approximation, "⚠️ **Approximation**", "Features approximated or weakened"),
                (FrictionType.Extension, "🔧 **Extension**", "Features requiring target-specific extensions")
            ).flatMap { case (frictionType, title, description) =>
                grouped.get(frictionType).map { typeEntries =>
                    val entriesMarkdown = typeEntries
                        .map { entry =>
                            val suggestionPart = entry.suggestion match
                                case Some(s) => s"\n  - **Suggestion**: $s"
                                case None    => ""
                            s"- **`${ entry.path }`**: ${ entry.message }$suggestionPart"
                        }
                        .mkString("\n")

                    s"""## $title
                       |
                       |$description
                       |
                       |$entriesMarkdown""".stripMargin
                }
            }

            val summary = s"""# Schema Translation Friction Report
                             |
                             |**Total Issues**: ${ entries.size }
                             |
                             |""".stripMargin

            summary + sections.mkString("\n\n")

    /** Renders the friction report as plain text (for console output).
      *
      * @return
      *   A plain text formatted string without Markdown formatting
      */
    def toPlainText: String =
        if isEmpty then "No translation issues detected."
        else
            val grouped  = entries.groupBy(_.frictionType)
            val sections = List(
                (FrictionType.Loss, "LOSS", "Features completely lost in translation"),
                (FrictionType.Approximation, "APPROXIMATION", "Features approximated or weakened"),
                (FrictionType.Extension, "EXTENSION", "Features requiring target-specific extensions")
            ).flatMap { case (frictionType, title, description) =>
                grouped.get(frictionType).map { typeEntries =>
                    val entriesText = typeEntries
                        .map { entry =>
                            val suggestionPart = entry.suggestion match
                                case Some(s) => s"\n    Suggestion: $s"
                                case None    => ""
                            s"  - ${ entry.path }: ${ entry.message }$suggestionPart"
                        }
                        .mkString("\n")

                    s"""$title: $description
                       |$entriesText""".stripMargin
                }
            }

            val summary = s"""Schema Translation Friction Report
                             |Total Issues: ${ entries.size }
                             |""".stripMargin

            summary + "\n" + sections.mkString("\n\n")

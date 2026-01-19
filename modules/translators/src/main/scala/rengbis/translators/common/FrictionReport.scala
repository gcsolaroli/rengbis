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

case class FrictionReport(
    entries: List[FrictionEntry] = List.empty,
    fetchedUrls: Set[String] = Set.empty // External URLs that were fetched during import
):
    def addLoss(path: String, message: String, suggestion: Option[String] = None): FrictionReport =
        copy(entries = entries :+ FrictionEntry(FrictionType.Loss, path, message, suggestion))

    def addApproximation(path: String, message: String, suggestion: Option[String] = None): FrictionReport =
        copy(entries = entries :+ FrictionEntry(FrictionType.Approximation, path, message, suggestion))

    def addExtension(path: String, message: String, suggestion: Option[String] = None): FrictionReport =
        copy(entries = entries :+ FrictionEntry(FrictionType.Extension, path, message, suggestion))

    def withFetchedUrls(urls: Set[String]): FrictionReport =
        copy(fetchedUrls = urls)

    def merge(other: FrictionReport): FrictionReport =
        copy(entries = entries ++ other.entries, fetchedUrls = fetchedUrls ++ other.fetchedUrls)

    def isEmpty: Boolean = entries.isEmpty && fetchedUrls.isEmpty

    def nonEmpty: Boolean = entries.nonEmpty || fetchedUrls.nonEmpty

    /** Simplifies a path for display by extracting meaningful segments.
      *
      * Handles two formats:
      *   - Definition paths: `defName → $/properties/field` becomes `defName → field`
      *   - Root paths: `$/allOf[0]/properties/target` becomes `allOf[0].target`
      */
    private def simplifyPath(path: String): String =
        // Check if this is a definition path (contains →)
        if path.contains(" → ") then
            val parts      = path.split(" → ", 2)
            val defName    = parts(0)
            val relPath    = if parts.length > 1 then parts(1) else "$"
            val simplified = simplifyPathSegments(relPath)
            if simplified == "$" then defName
            else s"$defName → $simplified"
        else simplifyPathSegments(path)

    /** Simplifies path segments by removing noise like $ and properties. */
    private def simplifyPathSegments(path: String): String =
        val segments   = path.split("/").toList
        val meaningful = segments.flatMap { segment =>
            segment match
                case "$"          => None // Skip root marker
                case "properties" => None // Skip "properties" - it's implied
                case s            => Some(s)
        }
        if meaningful.isEmpty then "$"
        else meaningful.mkString(".")

    /** Formats a list of paths for display as a sublist, one path per line. */
    private def formatPaths(paths: List[String], forMarkdown: Boolean, indent: String): String =
        val simplified = paths.map(simplifyPath).distinct
        if forMarkdown then simplified.map(p => s"$indent- `$p`").mkString("\n")
        else simplified.map(p => s"$indent- $p").mkString("\n")

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
                (FrictionType.Loss, "Loss", "Features completely lost in translation"),
                (FrictionType.Approximation, "Approximation", "Features approximated or weakened"),
                (FrictionType.Extension, "Extension", "Features requiring target-specific extensions")
            ).flatMap { case (frictionType, title, description) =>
                grouped.get(frictionType).map { typeEntries =>
                    val typeCount        = typeEntries.size
                    // Group by message within each friction type
                    val byMessage        = typeEntries.groupBy(_.message)
                    val messagesMarkdown = byMessage.toList
                        .sortBy(-_._2.size) // Sort by count descending
                        .map { case (message, msgEntries) =>
                            val msgCount       = msgEntries.size
                            val suggestion     = msgEntries.flatMap(_.suggestion).headOption
                            val suggestionPart = suggestion match
                                case Some(s) => s"\n  - **Suggestion**: $s"
                                case None    => ""
                            val pathsList      = formatPaths(msgEntries.map(_.path), forMarkdown = true, indent = "    ")
                            s"- **$message** ($msgCount)$suggestionPart\n  - Paths:\n$pathsList"
                        }
                        .mkString("\n")

                    s"""## $title ($typeCount)
                       |
                       |$description
                       |
                       |$messagesMarkdown""".stripMargin
                }
            }

            // Add external resources section if any URLs were fetched
            val externalSection =
                if fetchedUrls.isEmpty then ""
                else
                    val urlsList = fetchedUrls.toList.sorted.map(url => s"- $url").mkString("\n")
                    s"""
                       |
                       |## External Resources Fetched (${ fetchedUrls.size })
                       |
                       |The following external schemas were downloaded during import:
                       |
                       |$urlsList""".stripMargin

            val summary = s"""# Schema Translation Friction Report
                             |
                             |**Total Issues**: ${ entries.size }
                             |
                             |""".stripMargin

            summary + sections.mkString("\n\n") + externalSection

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
                    val typeCount    = typeEntries.size
                    // Group by message within each friction type
                    val byMessage    = typeEntries.groupBy(_.message)
                    val messagesText = byMessage.toList
                        .sortBy(-_._2.size) // Sort by count descending
                        .map { case (message, msgEntries) =>
                            val msgCount       = msgEntries.size
                            val suggestion     = msgEntries.flatMap(_.suggestion).headOption
                            val suggestionPart = suggestion match
                                case Some(s) => s"\n      Suggestion: $s"
                                case None    => ""
                            val pathsList      = formatPaths(msgEntries.map(_.path), forMarkdown = false, indent = "        ")
                            s"  - $message ($msgCount)$suggestionPart\n      Paths:\n$pathsList"
                        }
                        .mkString("\n")

                    s"""$title ($typeCount): $description
                       |$messagesText""".stripMargin
                }
            }

            // Add external resources section if any URLs were fetched
            val externalSection =
                if fetchedUrls.isEmpty then ""
                else
                    val urlsList = fetchedUrls.toList.sorted.map(url => s"  - $url").mkString("\n")
                    s"""
                       |
                       |EXTERNAL RESOURCES FETCHED (${ fetchedUrls.size }):
                       |The following external schemas were downloaded during import:
                       |$urlsList""".stripMargin

            val summary = s"""Schema Translation Friction Report
                             |Total Issues: ${ entries.size }
                             |""".stripMargin

            summary + "\n" + sections.mkString("\n\n") + externalSection

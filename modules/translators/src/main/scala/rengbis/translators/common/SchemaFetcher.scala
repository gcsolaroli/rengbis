package rengbis.translators.common

/** A trait for fetching schema content from external URLs.
  *
  * This is used by schema importers to resolve external references. Implementations can provide HTTP fetching, caching, authentication, etc.
  */
trait SchemaFetcher:
    /** Fetches schema content from a URL.
      *
      * @param url
      *   The URL to fetch
      * @return
      *   Either an error message or the schema content as a string
      */
    def fetch(url: String): Either[String, String]

object SchemaFetcher:
    /** A no-op fetcher that always fails. Used when no fetcher is configured. */
    val NoOp: SchemaFetcher = (url: String) => Left(s"No schema fetcher configured for URL: $url")

package rengbis.cli

import rengbis.translators.common.SchemaFetcher

import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.nio.file.{ Files, Path, Paths }
import java.time.Duration

/** HTTP-based schema fetcher for resolving external URL references in JSON Schema and XSD.
  *
  * Uses Java 11+ built-in HttpClient with:
  *   - Automatic redirect following
  *   - 10 second connect timeout
  *   - 30 second read timeout
  *   - Accept header for JSON and JSON Schema content types
  *
  * Also supports relative file paths for local schema references.
  */
object HttpSchemaFetcher extends SchemaFetcher:

    /** Creates a fetcher that resolves relative paths against a base directory */
    def withBasePath(basePath: Path): SchemaFetcher =
        new SchemaFetcher:
            def fetch(url: String): Either[String, String] =
                if url.startsWith("http://") || url.startsWith("https://") || url.startsWith("file://") then fetchUrl(url)
                else fetchLocalFile(url, Some(basePath))

    private val client = HttpClient
        .newBuilder()
        .followRedirects(HttpClient.Redirect.NORMAL)
        .connectTimeout(Duration.ofSeconds(10))
        .build()

    def fetch(url: String): Either[String, String] =
        // Check if this is a URL or a relative file path
        if url.startsWith("http://") || url.startsWith("https://") || url.startsWith("file://") then fetchUrl(url)
        else fetchLocalFile(url, None)

    private def fetchUrl(url: String): Either[String, String] =
        try
            val request = HttpRequest
                .newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(30))
                .header("Accept", "application/json, application/schema+json, application/xml, */*")
                .header("User-Agent", "rengbis-cli")
                .GET()
                .build()

            val response = client.send(request, HttpResponse.BodyHandlers.ofString())

            if response.statusCode() >= 200 && response.statusCode() < 300 then Right(response.body())
            else Left(s"HTTP ${ response.statusCode() }")
        catch
            case e: IllegalArgumentException => Left(s"Invalid URL: ${ e.getMessage }")
            case e: java.io.IOException      => Left(s"Network error: ${ e.getMessage }")
            case e: InterruptedException     =>
                Thread.currentThread().interrupt()
                Left("Request interrupted")
            case e: Exception                => Left(s"Unexpected error: ${ e.getMessage }")

    private def fetchLocalFile(path: String, basePath: Option[Path]): Either[String, String] =
        try
            val filePath = basePath match
                case Some(base) => base.resolve(path)
                case None       => Paths.get(path)
            if Files.exists(filePath) then Right(Files.readString(filePath))
            else Left(s"File not found: $filePath")
        catch case e: Exception => Left(s"Error reading file: ${ e.getMessage }")

package rengbis.cli

import rengbis.translators.common.SchemaFetcher

import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.time.Duration

/** HTTP-based schema fetcher for resolving external URL references in JSON Schema.
  *
  * Uses Java 11+ built-in HttpClient with:
  *   - Automatic redirect following
  *   - 10 second connect timeout
  *   - 30 second read timeout
  *   - Accept header for JSON and JSON Schema content types
  */
object HttpSchemaFetcher extends SchemaFetcher:

    private val client = HttpClient
        .newBuilder()
        .followRedirects(HttpClient.Redirect.NORMAL)
        .connectTimeout(Duration.ofSeconds(10))
        .build()

    def fetch(url: String): Either[String, String] =
        try
            val request = HttpRequest
                .newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(30))
                .header("Accept", "application/json, application/schema+json, */*")
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

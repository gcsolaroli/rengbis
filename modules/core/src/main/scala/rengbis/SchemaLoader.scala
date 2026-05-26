package rengbis

import java.nio.file.{ Files, Path }
import zio.Chunk
import rengbis.Schema.Schema

object SchemaLoader:

    def loadSchemaAtPath(path: Path): Either[String, ResolvedSchema] = parseSchemaAtPath(path)
        .flatMap(resolveImports(path, Set.empty))
        .flatMap(resolveReferences)

    /** Variant of [[loadSchemaAtPath]] that consumes the schema as an in-memory
      * string rather than reading it from disk. Relative `import` paths inside
      * `content` are resolved against `baseDir`.
      *
      * Use this when the schema text lives in a host document (e.g. a YAML
      * configuration block) and there's no dedicated `.rengbis` file on disk —
      * the host document's directory acts as the import anchor.
      *
      * Internally a synthetic schema path under `baseDir` is used as the
      * circular-import key; it never resolves to a real file, so it can't
      * collide with any actual `import` target.
      */
    def loadSchemaFromString(content: String, baseDir: Path): Either[String, ResolvedSchema] =
        val syntheticPath = baseDir.resolve("(inline-schema)")
        parseSchema(content)
            .flatMap(resolveImports(syntheticPath, Set.empty))
            .flatMap(resolveReferences)

    // ========================================================================
    //
    /** Read and parse a schema file. File-system failures (missing file,
      * unreadable, etc.) are folded into the returned `Left` instead of being
      * thrown — callers that compose this in `for` / `flatMap` chains keep a
      * clean error path.
      */
    def parseSchemaAtPath(path: Path): Either[String, ParsedSchema] =
        try parseSchema(Files.readString(path))
        catch
            case e: java.nio.file.NoSuchFileException => Left(s"schema file not found: ${ path }")
            case e: java.io.IOException               => Left(s"could not read schema file '${ path }': ${ e.getMessage }")
    def parseSchema(content: String): Either[String, ParsedSchema]  = Schema
        .parse(content)
        .flatMap(resolveParsedReferences)

    // ------------------------------------------------------------------------

    protected def resolveReferences(schema: ResolvedSchema): Either[String, ResolvedSchema] =
        for
            definitions <- sequenceEithers(schema.definitions.toSeq.map((scope, s) => s.replaceReferencedValues(schema.definitions.toSeq*).map(d => (scope, d))))
            root        <- optionEither(schema.root.map(s => s.replaceReferencedValues(definitions.toSeq*)))
        yield ResolvedSchema(root, definitions.toMap)

    protected def resolveParsedReferences(schema: ParsedSchema): Either[String, ParsedSchema] =
        for
            definitions <- sequenceEithers(schema.definitions.toSeq.map((scope, s) => s.replaceReferencedValues(schema.definitions.toSeq*).map(d => (scope, d))))
            root        <- optionEither(schema.root.map(s => s.replaceReferencedValues(definitions.toSeq*)))
        yield ParsedSchema(root, definitions.toMap, schema.imports)

    protected def resolveImports(schemaPath: Path, visited: Set[Path])(schema: ParsedSchema): Either[String, ResolvedSchema] =
        if visited.contains(schemaPath) then return Left(s"Circular import detected: ${ schemaPath }")

        val newVisited: Set[Path]                              = visited + schemaPath
        val importResults: Either[String, Map[String, Schema]] = sequenceEithers(
            schema.imports.map { (namespace, path) =>
                val importPath = schemaPath.getParent.resolve(path)
                for
                    parsed: ParsedSchema     <- parseSchemaAtPath(importPath)
                    imported: ResolvedSchema <- resolveImports(importPath, newVisited)(parsed)
                    scopedSchemas             = imported.definitions.toList.map((k, v) => (s"${ namespace }.${ k }", v))
                                                    ++ imported.root.map(r => (namespace, r)).toList
                yield scopedSchemas
            }.toSeq
        ).map(s => s.flatMap(identity).toMap)

        importResults.map(m => ResolvedSchema(schema.root, schema.definitions ++ m))

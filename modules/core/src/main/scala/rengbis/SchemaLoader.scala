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
            definitions <- resolveDefinitionsTransitively(schema.definitions)
            root        <- optionEither(schema.root.map(s => s.replaceReferencedValues(definitions.toSeq*)))
        yield ResolvedSchema(root, definitions)

    protected def resolveParsedReferences(schema: ParsedSchema): Either[String, ParsedSchema] =
        for
            definitions <- resolveDefinitionsTransitively(schema.definitions)
            root        <- optionEither(schema.root.map(s => s.replaceReferencedValues(definitions.toSeq*)))
        yield ParsedSchema(root, definitions, schema.imports)

    //  `replaceReferencedValues` does a single-level substitution: when it
    //  finds `NamedValueReference("X")` it pastes X's *current* schema in,
    //  but doesn't descend into the substituted value. For a chain
    //  `A -> B -> C` that means resolving A against the original definitions
    //  pulls in B's still-unresolved form (B with a `NamedValueReference("C")`
    //  inside). One pass isn't enough — we need to iterate to a fixed point.
    //
    //  This matters specifically across import boundaries: imports namespace
    //  the imported definitions (e.g. `Period` becomes `lib.Period`), but
    //  references inside those definitions remain un-namespaced. Anything
    //  not resolved before namespacing can't be resolved afterwards, because
    //  the importer's context has `lib.Period` while the inner schema still
    //  references the bare `Period`.
    //
    //  Cyclic schemas (e.g. recursive `Expression = Value | Operation` with
    //  `Operation.secondoOperand: Expression`) never converge — each pass
    //  expands the recursion one level deeper. We bound iterations and stop
    //  silently rather than erroring: the previous single-pass code already
    //  tolerated cycles by leaving leftover NamedValueReferences deep in the
    //  schema tree, and validation of shallow data didn't reach them. Keeping
    //  that behavior preserves backward compatibility with recursive samples.
    //
    //  Non-cyclic bound: `definitions.size + 1` passes — each pass resolves
    //  at least one more level of any chain that has somewhere to go.
    private def resolveDefinitionsTransitively(definitions: Map[String, Schema.Schema]): Either[String, Map[String, Schema.Schema]] =
        @scala.annotation.tailrec
        def iterate(current: Map[String, Schema.Schema], passesLeft: Int): Either[String, Map[String, Schema.Schema]] =
            sequenceEithers(current.toSeq.map((scope, s) => s.replaceReferencedValues(current.toSeq*).map(d => (scope, d)))) match
                case Left(err)   => Left(err)
                case Right(next) =>
                    val nextMap = next.toMap
                    if nextMap == current || passesLeft <= 0 then Right(nextMap)
                    else iterate(nextMap, passesLeft - 1)
        iterate(definitions, definitions.size + 1)

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

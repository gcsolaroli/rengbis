package rengbis

import java.nio.file.{ Files, Path }
import zio.Chunk
import rengbis.Schema.Schema

object SchemaLoader:

    def loadSchemaAtPath(path: Path): Either[String, ResolvedSchema] = parseSchemaAtPath(path)
        .flatMap(resolveImports(path, Set.empty))
        .flatMap(resolveReferences)

    // ========================================================================
    //
    def parseSchemaAtPath(path: Path): Either[String, ParsedSchema] = parseSchema(Files.readString(path))
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

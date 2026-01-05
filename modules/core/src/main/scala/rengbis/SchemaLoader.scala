package rengbis

import java.nio.file.{ Files, Path }
import zio.Chunk
import rengbis.Schema.{ ImportStatement, Schema, SchemaSyntax }

object SchemaLoader:

    case class LoadedSchema(
        path: Path,
        imports: Map[String, ImportStatement],
        definitions: Map[String, Schema],
        rootSchema: Option[Schema]
    )

    case class ResolvedSchema(
        allDefinitions: Map[String, Schema],
        rootSchema: Schema
    )

    def parseSchemaFile(content: String): Either[String, (Map[String, ImportStatement], Map[String, Schema], Option[Schema])] =
        SchemaSyntax.schema
            .parseString(content)
            .left
            .map(_.pretty)
            .map { (definitions, rootSchema) =>
                val imports   = definitions.collect { case (name, imp: ImportStatement) => (name, imp) }.toMap
                val namedDefs = definitions.collect { case (name, schema) if !schema.isInstanceOf[ImportStatement] => (name, schema) }.toMap
                (imports, namedDefs, rootSchema.map(_._2))
            }

    def loadSchemaFile(path: Path): Either[String, LoadedSchema] =
        for
            content <- Either.cond(Files.exists(path), Files.readString(path), s"File not found: $path")
            parsed  <- parseSchemaFile(content)

            (imports, definitions, rootSchema) = parsed
        yield LoadedSchema(path, imports, definitions, rootSchema)

    def resolveImports(schema: LoadedSchema, visited: Set[Path] = Set.empty): Either[String, Map[String, Schema]] =
        if visited.contains(schema.path) then return Left(s"Circular import detected: ${ schema.path }")

        val newVisited    = visited + schema.path
        val importResults = Schema
            .sequenceEithers(
                schema.imports.map { (namespace, importStmt) =>
                    val importPath = schema.path.getParent.resolve(importStmt.path)
                    for
                        imported            <- loadSchemaFile(importPath)
                        importedWithImports <- resolveImports(imported, newVisited)
                        resolved            <- resolveInternalReferences(imported, importedWithImports)
                    yield (namespace, resolved)
                }.toSeq
            )

        importResults.flatMap { importedNamespaces =>
            var globalContext = Map.empty[String, Schema]
            globalContext ++= schema.definitions
            schema.rootSchema.foreach { root => globalContext += ("root" -> root) }

            importedNamespaces.foreach { (namespace, importedDefs) =>
                importedDefs.foreach { (name, defSchema) => if name != "root" then globalContext += (s"$namespace.$name" -> defSchema) }
                importedDefs.get("root").foreach { rootSchema => globalContext += (namespace -> rootSchema) }
            }
            Right(globalContext)
        }

    private def resolveInternalReferences(schema: LoadedSchema, contextWithImports: Map[String, Schema]): Either[String, Map[String, Schema]] =
        var localContext = contextWithImports
        schema.rootSchema.foreach { root => localContext += ("root" -> root) }
        Schema
            .sequenceEithers(localContext.map { (name, defSchema) =>
                defSchema.replaceReferencedValues(localContext.toSeq*).map(resolved => (name, resolved))
            }.toSeq)
            .map(_.toMap)

    def loadAndResolve(path: Path): Either[String, ResolvedSchema] =
        for
            loaded          <- loadSchemaFile(path)
            globalContext   <- resolveImports(loaded)
            rootSchema      <- loaded.rootSchema.toRight(s"No root schema defined in $path")
            resolvedRoot    <- rootSchema.replaceReferencedValues(globalContext.toSeq*)
            resolvedContext <- Schema
                                   .sequenceEithers(globalContext.map { (name, schema) =>
                                       schema.replaceReferencedValues(globalContext.toSeq*).map(resolved => (name, resolved))
                                   }.toSeq)
                                   .map(_.toMap)
        yield ResolvedSchema(resolvedContext, resolvedRoot)

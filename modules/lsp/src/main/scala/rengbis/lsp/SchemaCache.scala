package rengbis.lsp

import java.nio.file.Path
import scala.collection.concurrent.TrieMap
import rengbis.SchemaLoader
import rengbis.SchemaLoader.ResolvedSchema

class SchemaCache:
    private val cache = TrieMap[Path, ResolvedSchema]()

    def invalidate(schemaPath: Path): Unit = cache.remove(schemaPath)
    def invalidateAll(): Unit              = cache.clear()

    def getOrLoad(schemaPath: Path): Either[String, ResolvedSchema] =
        cache.get(schemaPath) match
            case Some(schema) => Right(schema)
            case None         =>
                SchemaLoader
                    .loadAndResolve(schemaPath)
                    .map: schema =>
                        cache.put(schemaPath, schema)
                        schema
